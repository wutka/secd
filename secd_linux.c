#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "secd.h"

extern CELL *S, *C;
extern unsigned char code[MAX_CODE_SIZE];

void print_cell(CELL *cell);

void panic(char *message) {
    printf("%s\n", message);
    fflush(stdout);
    exit(1);
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
    int i, ch;
    FILE *infile;

    if (argc < 2) {
        printf("Please supply a filename\n");
        return 0;
    }

    if ((infile = fopen(argv[1], "rb")) == NULL) {
        perror("fopen");
        return 0;
    }

    i = 0;
    while ((ch = fgetc(infile)) != EOF) {
        if (i >= MAX_CODE_SIZE) {
            panic("No more code space");
        }
        code[i++] = (unsigned char) (ch & 0xff);
    }

    initialize_pool();

    C = make_cons_cell(make_int_cell(0), make_nil_cell());

    execute();

    printf("\nFinal stack:\n");
    print_cell(S);
    printf("\n");
}
