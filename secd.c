#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "secd.h"

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
    "DUM", "RAP", "STOP", "CGE", "CGT", "CEQ", "CNE", "CLE", "CLT", "TSEL" };

CELL cell_pool[MAX_CELLS];

CELL *free_list = NULL;

CELL *S = NULL;
CELL *E = NULL;
CELL *C = NULL;
CELL *D = NULL;

unsigned char code[MAX_CODE_SIZE];

extern void print_cell(CELL *cell);
extern void panic(char *message);

int compute_offset(CELL *cell) {
    if (cell == NULL) {
        return 0;
    }

    return cell - cell_pool;
}

CELL *cell_for_offset(int offset) {
    if (offset == 0) {
        return NULL;
    }
    return &cell_pool[offset];
}

void initialize_pool() {
    for (int i=1; i < MAX_CELLS-1; i++) {
        cell_pool[i].data = compute_offset(&cell_pool[i+1]);
    }
    cell_pool[MAX_CELLS-1].data = 0;
    free_list = &cell_pool[1];
}

void free_cell(CELL *cell) {
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

void mark_cells(CELL *cell) {
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

void mark() {
    mark_cells(S);
    mark_cells(E);
    mark_cells(C);
    mark_cells(D);
    mark_cells(free_list);
}

void sweep() {
    for (int i=1; i < MAX_CELLS; i++) {
        if (!cell_pool[i].tag) {
            cell_pool[i].data = compute_offset(free_list);
            free_list = &cell_pool[i];
#ifdef DEBUG
            printf("Freed cell %d\n", i);
#endif
        } else {
            cell_pool[i].tag = 0;
        }
    }
}

void collect_garbage() {
    mark();
    sweep();
#ifdef DEBUG
    printf("\nCollected Garbage\n");
    fflush(stdout);
#endif
}

CELL *alloc_cell() {
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

CELL *make_int_cell(int i) {
    CELL *new_cell;

    new_cell = alloc_cell();
    new_cell->cell_type = TYPE_INT;
    new_cell->data = i;

    return new_cell;
}

CELL *make_nil_cell() {
    CELL *new_cell;

    new_cell = alloc_cell();
    new_cell->cell_type = TYPE_NIL;
    new_cell->data = 0;

    return new_cell;
}

CELL *make_cons_cell(CELL *cell_car, CELL *cell_cdr) {
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

int car_int(CELL *cell) {
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

CELL *car_cell(CELL *cell) {
    if (cell == NULL) {
        panic("Try to get CAR of nil");
    }
    if (cell->cell_type != TYPE_CONS) {
        panic("Tried to CAR non-CONS");
    }
    
    return cell_for_offset(CAR_OFFSET(cell));
}

int cdr_int(CELL *cell) {
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

CELL *cdr_cell(CELL *cell) {
    if (cell == NULL) {
        panic("Tried to get CDR of nil");
    }
    if (cell->cell_type != TYPE_CONS) {
        panic("Expected cell CONS for CDR");
    }
    return cell_for_offset(CDR_OFFSET(cell));
}

CELL *locate(int env_num, int env_offset) {
    CELL *curr_pos;

    curr_pos = E;
    while (env_num > 0) {
        curr_pos = cdr_cell(curr_pos);
        if (curr_pos == NULL) {
            panic("Invalid environment reference");
        }
        env_num--;
    }

    curr_pos = car_cell(curr_pos);
    while (env_offset > 0) {
        curr_pos = cdr_cell(curr_pos);
        if (curr_pos == NULL) {
            panic("Invalid environment offset");
        }
        env_offset--;
    }
    return car_cell(curr_pos);
}

void set_code_pos(int new_pos) {
    CELL *code_pos_cell;

    if (C == NULL) {
        return;
    }

    code_pos_cell = car_cell(C);
    code_pos_cell->data = (unsigned long) new_pos;
}

void execute() {
    int instr, x, y, z, env_num, env_offset, code_pos, t, f;
    CELL *loc, *loc2, *loc3;

    while (C != NULL) {
#ifdef DEBUG
        printf("S: ");
        print_cell(S);
        printf("  E: ");
        print_cell(E);
        printf("  C: ");
        print_cell(C);
        printf("  D: ");
        print_cell(D);
        printf("\n");
#endif

        code_pos = car_int(C);
        instr = code[code_pos++];
        set_code_pos(code_pos);

#ifdef DEBUG
        printf("Instr %s\n", instrs[instr]);
#endif
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

                y = car_int(S);
                S = cdr_cell(S);

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

CELL *reverse(CELL *lst) {
    CELL *new_list = NULL;

    while (lst != NULL) {
        new_list = make_cons_cell(car_cell(lst), new_list);
        lst = cdr_cell(lst);
    }

    return new_list;
}
