#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define MAX_CELLS 2000

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

#define CAR_OFFSET(c) ((c->data >> 16) & 0xffff)
#define CDR_OFFSET(c) (c->data & 0xffff)

CELL cell_pool[MAX_CELLS];

CELL *free_list = NULL;

CELL *S = NULL;
CELL *E = NULL;
CELL *C = NULL;
CELL *D = NULL;

void panic(char *message) {
    printf("%s\n", message);
    fflush(stdout);
    exit(1);
}

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
            printf("Freed cell %d\n", i);
        } else {
            cell_pool[i].tag = 0;
        }
    }
}

void collect_garbage() {
    mark();
    sweep();
    printf("\nCollected Garbage\n");
    fflush(stdout);
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

void execute() {
    int instr, x, y, z, env_num, env_offset;
    CELL *loc, *loc2, *loc3;

    while (C != NULL) {
        instr = car_int(C);
        C = cdr_cell(C);

        switch (instr) {
            case INSTR_NIL:
                S = make_cons_cell(make_nil_cell(), S);
                break;
            case INSTR_LDC:
                x = car_int(C);
                C = cdr_cell(C);
                S = make_cons_cell(make_int_cell(x), S);
                break;
            case INSTR_LD:
                loc = car_cell(C);
                C = cdr_cell(C);

                env_num = car_int(loc);
                env_offset = cdr_int(loc);

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
                y = cdr_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(x+y), S);
                break;

            case INSTR_SUB:
                x = car_int(S);
                S = cdr_cell(S);
                y = cdr_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(x-y), S);
                break;

            case INSTR_MUL:
                x = car_int(S);
                S = cdr_cell(S);
                y = cdr_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(x*y), S);
                break;

            case INSTR_DIV:
                x = car_int(S);
                S = cdr_cell(S);
                y = cdr_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(x/y), S);
                break;

            case INSTR_MOD:
                x = car_int(S);
                S = cdr_cell(S);
                y = cdr_int(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_int_cell(x%y), S);
                break;

            case INSTR_SEL:
                x = car_int(S);
                S = cdr_cell(S);

                loc = car_cell(S);
                S = cdr_cell(S);

                loc2 = car_cell(S);
                S = cdr_cell(S);

                D = make_cons_cell(C, D);
                if (x) {
                    C = make_cons_cell(loc, C);
                } else {
                    C = make_cons_cell(loc2, C);
                }
                break;

            case INSTR_JOIN:
                C = car_cell(D);
                D = cdr_cell(D);
                break;

            case INSTR_LDF:
                loc = car_cell(S);
                S = cdr_cell(S);

                S = make_cons_cell(make_cons_cell(loc, E), S);
                break;

            case INSTR_AP:
                loc = car_cell(S);
                S = cdr_cell(S);

                loc2 = car_cell(S);
                S = cdr_cell(S);

                D = make_cons_cell(S, make_cons_cell(E, make_cons_cell(C, D)));

                S = make_nil_cell();
                E = make_cons_cell(loc2, cdr_cell(loc));
                C = car_cell(loc);
                break;

            case INSTR_DUM:
                E = make_cons_cell(make_nil_cell(), E);
                break;

            case INSTR_RAP:
                loc = car_cell(S); // f.(nil.e)
                S = cdr_cell(S);
                loc2 = car_cell(S); // v
                S = cdr_cell(S);

                loc3 = car_cell(loc); // loc3 = f
                loc = make_cons_cell(loc2, cdr_cell(loc));

                D = make_cons_cell(S, make_cons_cell(E, make_cons_cell(C, D)));
                S = NULL;
                E = make_cons_cell(loc, E);
                C = loc3;
                break;

            case INSTR_STOP:
                return;
        }

    }

}

void print_cell(CELL *cell) {
    int printed_first;
    if (cell == NULL) return;

    switch (cell->cell_type) {
        case TYPE_INT:
            printf("%d", (int) cell->data);
            break;
        case TYPE_NIL:
            printf("NIL");
            break;

        case TYPE_CONS:
            printf("(");
            printed_first = 0;
            while (cell != NULL) {
                if (printed_first) printf(" ");
                print_cell(cell_for_offset(CAR_OFFSET(cell)));
                printed_first = 1;
                cell = cell_for_offset(CDR_OFFSET(cell));
                if (cell == NULL) break;
                if (cell->cell_type == TYPE_INT) {
                    printf(" . %d", (int) cell->data);
                    break;
                }
            }
            printf(")");
            break;
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

CELL *read_sexpr() {
    char ch;
    int num, in_num;
    CELL *curr_list;

    curr_list = NULL;

    ch = getchar();
    if (ch != '(') {
        panic("Expected ( to start sexpr");
    }

    num = 0;
    in_num = 0;
    while (1) {
        ch = getchar();
        if ((ch >= '0') && (ch <= '9')) {
            if (!in_num) {
                in_num = 1;
                num = 0;
            }
            num = num * 10 + (ch - '0');
            in_num = 1;
        } else if (ch == ' ') {
            if (in_num) {
                curr_list = make_cons_cell(make_int_cell(num), curr_list);
                in_num = 0;
            }
        } else if (ch == ')') {
            if (in_num) {
                curr_list = make_cons_cell(make_int_cell(num), curr_list);
                in_num = 0;
            }
            return reverse(curr_list);
        } else if (ch == '(') {
            ungetc('(', stdin);
            curr_list = make_cons_cell(read_sexpr(), curr_list);
        } else if (ch == '\n') {
        } else {
            printf("Unknown character: %c\n", ch);
        }
    }
}

void skip_newline() {
    char ch;

    ch = getchar();
    while (ch == '\n') {
        ch = getchar();
    }
    ungetc(ch, stdin);
}

int main(int argc, char *argv[]) {
    CELL *foo, *bar, *baz, *the_list;
    char ch;

    printf("cell size = %ld, cell pool size = %ld\n",
            sizeof(CELL), sizeof(cell_pool));

    initialize_pool();

    while (1) {
        printf("S = ");
        print_cell(S);
        printf("\nE = ");
        print_cell(E);
        printf("\nC = ");
        print_cell(C);
        printf("\nD = ");
        print_cell(D);
        printf("\n");
        printf("Command: ");
        fflush(stdout);
        ch = getchar();
        if (ch == 'R') {
            execute();
            printf("\nFinal stack:\n");
            print_cell(S);
            printf("\n");
        } else if (ch == 'S') {
            S = read_sexpr();
        } else if (ch == 'E') {
            E = read_sexpr();
        } else if (ch == 'C') {
            C = read_sexpr();
        } else if (ch == 'D') {
            D = read_sexpr();
        } else if (ch == 'Q') {
            break;
            continue;
        } else {
            printf("Unknown command: %c\n", ch);
        }
        skip_newline();
    }


}
