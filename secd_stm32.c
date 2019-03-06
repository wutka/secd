#include "mbed.h"
#include <stdint.h>

#define MAX_CELLS 500
#define MAX_CODE_SIZE 1000

typedef struct _CELL {
    unsigned char tag;
    unsigned char cell_type;
    unsigned char unused0;
    unsigned char unused1;
    unsigned long data;
} CELL;

#define TYPE_CONS 0
#define TYPE_INT 1
#define TYPE_NIL 2

#define INSTR_NIL  0
#define INSTR_LDC  1
#define INSTR_LD   2
#define INSTR_ATOM 3
#define INSTR_CAR  4
#define INSTR_CDR  5
#define INSTR_CONS 6
#define INSTR_ADD  7
#define INSTR_SUB  8
#define INSTR_MUL  9
#define INSTR_DIV  10
#define INSTR_MOD  11
#define INSTR_SEL  12
#define INSTR_JOIN 13
#define INSTR_LDF  14
#define INSTR_AP   15
#define INSTR_RTN  16
#define INSTR_DUM  17
#define INSTR_RAP  18
#define INSTR_STOP 19
#define INSTR_CGE  20
#define INSTR_CGT  21
#define INSTR_CEQ  22
#define INSTR_CNE  23
#define INSTR_CLE  24
#define INSTR_CLT  25
#define INSTR_TSEL 26

char *instrs[27] = { "NIL", "LDC", "LD", "ATOM", "CAR", "CDR", "CONS",
                     "ADD", "SUB", "MUL", "DIV", "MOD", "SEL", "JOIN", "LDF", "AP", "RTN",
                     "DUM", "RAP", "STOP", "CGE", "CGT", "CEQ", "CNE", "CLE", "CLT", "TSEL"
                   };

#define CAR_OFFSET(c) ((c->data >> 16) & 0xffff)
#define CDR_OFFSET(c) (c->data & 0xffff)

CELL cell_pool[MAX_CELLS];

CELL *free_list = NULL;

CELL *S = NULL;
CELL *E = NULL;
CELL *C = NULL;
CELL *D = NULL;

unsigned char code[MAX_CODE_SIZE];

void print_cell(CELL *cell);

Serial pc(USBTX, USBRX);

extern "C" void mbed_reset();

void panic(char *message)
{
    pc.printf("%s\r\n", message);
    NVIC_SystemReset();
}

int compute_offset(CELL *cell)
{
    if (cell == NULL) {
        return 0;
    }

    return cell - cell_pool;
}

CELL *cell_for_offset(int offset)
{
    if (offset == 0) {
        return NULL;
    }
    return &cell_pool[offset];
}

void initialize_pool()
{
    for (int i=1; i < MAX_CELLS-1; i++) {
        cell_pool[i].data = compute_offset(&cell_pool[i+1]);
    }
    cell_pool[MAX_CELLS-1].data = 0;
    free_list = &cell_pool[1];
}

void free_cell(CELL *cell)
{
    switch (cell->cell_type) {
        case TYPE_INT:
        case TYPE_NIL:
            break;
        case TYPE_CONS:
            if (cell->data >> 16 > 0) {
                free_cell(&cell_pool[cell->data >> 16]);
            }
            if (cell->data & 0xffff > 0) {
                free_cell(&cell_pool[cell->data & 0xffff]);
            }
            break;
    }
    cell->data = (long) free_list;
    free_list = cell;
}

void mark_cells(CELL *cell)
{
    if (cell == NULL) {
        return;
    }
    if (cell->tag == 1) {
        return;
    }
    cell->tag = 1;
    if (cell->cell_type == TYPE_CONS) {
        mark_cells(&cell_pool[cell->data >> 16]);
        mark_cells(&cell_pool[cell->data & 0xffff]);
    }
}

void mark()
{
    mark_cells(S);
    mark_cells(E);
    mark_cells(C);
    mark_cells(D);
    mark_cells(free_list);
}

void sweep()
{
    for (int i=1; i < MAX_CELLS; i++) {
        if (!cell_pool[i].tag) {
            cell_pool[i].data = compute_offset(free_list);
            free_list = &cell_pool[i];
        } else {
            cell_pool[i].tag = 0;
        }
    }
}

void collect_garbage()
{
    mark();
    sweep();
}

CELL *alloc_cell()
{
    CELL *new_cell;

    if (free_list == NULL) {
        collect_garbage();
    }

    new_cell = free_list;

    if (new_cell != NULL) {
        free_list = &cell_pool[new_cell->data];
    } else {
        panic("out of memory");
    }

    return new_cell;
}

CELL *make_int_cell(int i)
{
    CELL *new_cell;

    new_cell = alloc_cell();
    new_cell->cell_type = TYPE_INT;
    new_cell->data = i;

    return new_cell;
}

CELL *make_nil_cell()
{
    CELL *new_cell;

    new_cell = alloc_cell();
    new_cell->cell_type = TYPE_NIL;
    new_cell->data = 0;

    return new_cell;
}

CELL *make_cons_cell(CELL *cell_car, CELL *cell_cdr)
{
    CELL *new_cell;
    int car_offset, cdr_offset;

    new_cell = alloc_cell();
    new_cell->cell_type = TYPE_CONS;

    car_offset = compute_offset(cell_car);

    if ((cell_cdr != NULL) && (cell_cdr->cell_type == TYPE_NIL)) {
        cdr_offset = 0;
    } else {
        cdr_offset = compute_offset(cell_cdr);
    }

    new_cell->data = (car_offset << 16) | (cdr_offset);

    return new_cell;
}

int car_int(CELL *cell)
{
    if (cell == NULL) {
        panic("Try to get CAR of nil");
    }
    if (cell->cell_type != TYPE_CONS) {
        panic("Tried to CAR non-CONS");
    }

    CELL *car_cell = cell_for_offset(CAR_OFFSET(cell));
    if (car_cell == NULL) {
        panic("Tried to get CAR of nil cell");
    }
    if (car_cell->cell_type != TYPE_INT) {
        panic("Tried to get int CAR of non-int cell");
    }

    return (int) car_cell->data;
}

CELL *car_cell(CELL *cell)
{
    if (cell == NULL) {
        panic("Try to get CAR of nil");
    }
    if (cell->cell_type != TYPE_CONS) {
        panic("Tried to CAR non-CONS");
    }

    return cell_for_offset(CAR_OFFSET(cell));
}

int cdr_int(CELL *cell)
{
    if (cell == NULL) {
        panic("Try to get CDR of nil");
    }
    if (cell->cell_type != TYPE_CONS) {
        panic("Tried to CDR non-CONS");
    }

    CELL *cdr_cell = cell_for_offset(CDR_OFFSET(cell));
    if (cdr_cell == NULL) {
        panic("Tried to get CDR of nil cell");
    }
    if (cdr_cell->cell_type != TYPE_INT) {
        panic("Tried to get int CDR of non-int cell");
    }

    return (int) cdr_cell->data;
}

CELL *cdr_cell(CELL *cell)
{
    if (cell == NULL) {
        panic("Tried to get CDR of nil");
    }
    if (cell->cell_type != TYPE_CONS) {
        panic("Expected cell CONS for CDR");
    }
    return cell_for_offset(CDR_OFFSET(cell));
}

CELL *locate(int env_num, int env_offset)
{
    CELL *curr_pos;

    curr_pos = E;
    while (env_num > 0) {
        curr_pos = cdr_cell(curr_pos);
        if (curr_pos == NULL) {
            panic("Invalid environment reference");
        }
    }

    curr_pos = car_cell(curr_pos);
    while (env_offset > 0) {
        curr_pos = cdr_cell(curr_pos);
        if (curr_pos == NULL) {
            panic("Invalid environment offset");
        }
    }
    return car_cell(curr_pos);
}

void set_code_pos(int new_pos)
{
    CELL *code_pos_cell;

    if (C == NULL) {
        return;
    }

    code_pos_cell = car_cell(C);
    code_pos_cell->data = (unsigned long) new_pos;
}

void execute()
{
    int instr, x, y, env_num, env_offset, code_pos, t, f;
    CELL *loc, *loc2;

    while (C != NULL) {
        code_pos = car_int(C);
        instr = code[code_pos++];
        set_code_pos(code_pos);

        switch (instr) {
            case INSTR_NIL:
                S = make_cons_cell(make_nil_cell(), S);
                break;

            case INSTR_LDC:
                y = 0;
                for (int i=0; i < 4; i++) {
                    x = code[code_pos++];
                    y = (y << 8) + x;
                }
                set_code_pos(code_pos);

                S = make_cons_cell(make_int_cell(x), S);
                break;

            case INSTR_LD:
                env_num = code[code_pos++];
                env_offset = code[code_pos++];
                set_code_pos(code_pos);

                S = make_cons_cell(locate(env_num, env_offset), S);
                break;

            case INSTR_ATOM:
                loc = car_cell(S);
                S = cdr_cell(S);

                x = 0;
                if ((loc != NULL) && (loc->cell_type == TYPE_INT)) {
                    x = 1;
                }
                S = make_cons_cell(make_int_cell(x), S);
                break;

            case INSTR_CAR:
                loc = car_cell(S);
                S = cdr_cell(S);

                if (loc == NULL) {
                    panic("Tried to take CAR of NULL");
                }

                S = make_cons_cell(cell_for_offset(CAR_OFFSET(loc)), S);
                break;

            case INSTR_CDR:
                loc = car_cell(S);
                S = cdr_cell(S);

                if (loc == NULL) {
                    panic("Tried to take CDR of NULL");
                }

                S = make_cons_cell(cell_for_offset(CDR_OFFSET(loc)), S);
                break;

            case INSTR_CONS:
                loc = car_cell(S);
                S = cdr_cell(S);

                loc2 = car_cell(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_cons_cell(loc, loc2), S);
                break;

            case INSTR_ADD:
                x = car_int(S);
                S = cdr_cell(S);

                print_cell(S);
                printf("\n");

                y = car_int(S);
                S = cdr_cell(S);

                print_cell(S);
                printf("\n");

                S = make_cons_cell(make_int_cell(x+y), S);
                break;

            case INSTR_SUB:
                x = car_int(S);
                S = cdr_cell(S);
                y = car_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(y-x), S);
                break;

            case INSTR_MUL:
                x = car_int(S);
                S = cdr_cell(S);
                y = car_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(x*y), S);
                break;

            case INSTR_DIV:
                x = car_int(S);
                S = cdr_cell(S);
                y = car_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(y/x), S);
                break;

            case INSTR_MOD:
                x = car_int(S);
                S = cdr_cell(S);
                y = car_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(y%x), S);
                break;

            case INSTR_CGT:
                x = car_int(S);
                S = cdr_cell(S);
                y = car_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(y>x), S);
                break;

            case INSTR_CGE:
                x = car_int(S);
                S = cdr_cell(S);
                y = car_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(y>=x), S);
                break;

            case INSTR_CEQ:
                x = car_int(S);
                S = cdr_cell(S);
                y = car_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(x==y), S);
                break;

            case INSTR_SEL:
                x = car_int(S);
                S = cdr_cell(S);

                t = 0;
                for (int i=0; i < 4; i++) {
                    y = code[code_pos++];
                    t = (t << 8) + y;
                }
                f = 0;
                for (int i=0; i < 4; i++) {
                    y = code[code_pos++];
                    f = (f << 8) + y;
                }
                set_code_pos(code_pos);

                D = make_cons_cell(C, D);
                if (x) {
                    C = make_cons_cell(make_int_cell(t), C);
                } else {
                    C = make_cons_cell(make_int_cell(f), C);
                }
                break;

            case INSTR_TSEL:
                x = car_int(S);
                S = cdr_cell(S);

                t = 0;
                for (int i=0; i < 4; i++) {
                    y = code[code_pos++];
                    t = (t << 8) + y;
                }
                f = 0;
                for (int i=0; i < 4; i++) {
                    y = code[code_pos++];
                    f = (f << 8) + y;
                }
                set_code_pos(code_pos);

                if (x) {
                    C = make_cons_cell(make_int_cell(t), C);
                } else {
                    C = make_cons_cell(make_int_cell(f), C);
                }
                break;

            case INSTR_JOIN:
                C = car_cell(D);
                D = cdr_cell(D);
                break;

            case INSTR_LDF:
                y = 0;
                for (int i=0; i < 4; i++) {
                    x = code[code_pos++];
                    y = (y << 8) + x;
                }

                set_code_pos(code_pos);

                S = make_cons_cell(make_cons_cell(make_int_cell(y), E), S);
                break;

            case INSTR_AP:
                loc = car_cell(S);
                S = cdr_cell(S);

                x = code[code_pos++];
                set_code_pos(code_pos);

                loc2 = make_nil_cell();
                for (int i=0; i < x; i++) {
                    loc2 = make_cons_cell(car_cell(S), loc2);
                    S = cdr_cell(S);
                }

                D = make_cons_cell(S, make_cons_cell(E, make_cons_cell(C, D)));

                S = make_nil_cell();
                E = make_cons_cell(loc2, cdr_cell(loc));
                C = make_cons_cell(make_int_cell(car_int(loc)), make_nil_cell());

                break;


            case INSTR_RTN:
                if (D == NULL) return;

                loc = car_cell(S);

                S = make_cons_cell(loc, car_cell(D));
                D = cdr_cell(D);

                E = car_cell(D);
                D = cdr_cell(D);

                C = car_cell(D);
                D = cdr_cell(D);
                break;

            case INSTR_DUM:
                x = code[code_pos++];
                set_code_pos(code_pos);

                loc = make_nil_cell();
                for (int i=0; i < x; i++) {
                    loc = make_cons_cell(make_int_cell(0), loc);
                }

                E = make_cons_cell(loc, E);
                break;

            case INSTR_RAP:
                loc = car_cell(S);
                S = cdr_cell(S);

                x = code[code_pos++];
                set_code_pos(code_pos);

                loc2 = make_nil_cell();
                for (int i=0; i < x; i++) {
                    loc2 = make_cons_cell(car_cell(S), loc2);
                    S = cdr_cell(S);
                }
                E = cdr_cell(E);
                E = make_cons_cell(loc2, E);

                D = make_cons_cell(S, make_cons_cell(E, make_cons_cell(C, D)));

                S = make_nil_cell();
                E = make_cons_cell(loc2, cdr_cell(loc));
                C = make_cons_cell(make_int_cell(car_int(loc)), make_nil_cell());

                break;

            case INSTR_STOP:
                return;
        }

    }

}

void print_cell(CELL *cell)
{
    int printed_first;
    if (cell == NULL) return;

    switch (cell->cell_type) {
        case TYPE_INT:
            pc.printf("%d", (int) cell->data);
            break;
        case TYPE_NIL:
            pc.printf("NIL");
            break;

        case TYPE_CONS:
            pc.printf("(");
            printed_first = 0;
            while (cell != NULL) {
                if (printed_first) printf(" ");
                print_cell(cell_for_offset(CAR_OFFSET(cell)));
                printed_first = 1;
                cell = cell_for_offset(CDR_OFFSET(cell));
                if (cell == NULL) break;
                if (cell->cell_type == TYPE_INT) {
                    pc.printf(" . %d", (int) cell->data);
                    break;
                }
            }
            pc.printf(")");
            break;
    }
}

CELL *reverse(CELL *lst)
{
    CELL *new_list = NULL;

    while (lst != NULL) {
        new_list = make_cons_cell(car_cell(lst), new_list);
        lst = cdr_cell(lst);
    }

    return new_list;
}

int main()
{
    int ch, ch2, b, code_pos;
    int reading, wait_for_colon;

    pc.baud(115200);
    
    while (1) {
        code_pos = 0;
        reading = 0;
        wait_for_colon = 0;
        while (1) {
            if (reading) {
                ch = pc.getc();
                if (ch == '<') break;

                ch2 = pc.getc();
                pc.printf("%c%c", ch, ch2);
                if ((ch >= '0') && (ch <= '9')) {
                    ch -= '0';
                } else if ((ch >= 'A') && (ch <= 'F')) {
                    ch = 10 + ch - 'A';
                } else if ((ch >= 'a') && (ch <= 'f')) {
                    ch = 10 + ch - 'a';
                } else {
                    panic("Invalid character received");
                }
                if ((ch2 >= '0') && (ch2 <= '9')) {
                    ch2 -= '0';
                } else if ((ch2 >= 'A') && (ch2 <= 'F')) {
                    ch2 = 10 + ch2 - 'A';
                } else if ((ch2 >= 'a') && (ch2 <= 'f')) {
                    ch2 = 10 + ch2 - 'a';
                } else {
                    panic("Invalid character received");
                }
                b = (ch << 4) + ch2;
                code[code_pos++] = b;
            } else {
                ch = pc.getc();
                if (ch == '>') {
                    wait_for_colon = 1;
                } else if ((ch == ':') && wait_for_colon) {
                    reading = 1;
                    wait_for_colon = 0;
                } else if ((ch == '\n') || (ch == '\r')) {
                    pc.printf("SECD Machine\r\n");
                    wait_for_colon = 0;
                } else {
                    pc.printf("Unexpected char - %c\r\n", ch);
                }
            }
        }

        S = NULL;
        E = NULL;
        C = NULL;
        D = NULL;
        
        initialize_pool();

        C = make_cons_cell(make_int_cell(0), make_nil_cell());

        execute();

        pc.printf("\r\nFinal stack:\r\n");
        print_cell(S);
        pc.printf("\r\n");
    }
}
