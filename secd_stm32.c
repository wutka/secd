#include "mbed.h"
#include <stdint.h>

#include "secd.h"

void print_cell(CELL *cell);

Serial pc(USBTX, USBRX);

extern "C" void mbed_reset();

void panic(char *message)
{
    pc.printf("%s\r\n", message);
    NVIC_SystemReset();
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
