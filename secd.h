void initialize_pool();
void execute();

typedef struct _CELL {
    unsigned char tag;
    unsigned char cell_type;
    unsigned char unused0;
    unsigned char unused1;
    unsigned long data;
} CELL;

CELL *make_cons_cell(CELL *, CELL*);
CELL *make_int_cell(int);
CELL *make_nil_cell();
CELL *cell_for_offset(int);
CELL *reverse(CELL *);

#define TYPE_CONS 0
#define TYPE_INT 1
#define TYPE_NIL 2

#define CAR_OFFSET(c) ((c->data >> 16) & 0xffff)
#define CDR_OFFSET(c) (c->data & 0xffff)

#define MAX_CELLS 1000
#define MAX_CODE_SIZE 1000

