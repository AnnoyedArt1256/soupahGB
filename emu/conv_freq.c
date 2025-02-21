#include <stdio.h>
#include <stdint.h>

uint32_t noise_sdsp[32] = {
    0, 2048, 1536, 1280, 1024, 768, 640, 512, 384, 320, 256, 192, 160, 128, 65, 80,
    64, 48, 40, 32, 24, 20, 16, 12, 10, 8, 6, 5, 4, 3, 2, 1
};

int main() {
    printf("snestabl_lo:\n.byte ");
    for (int f = 0; f < 2048; f++) {
        double freq = 4194304.0/((double)(2048-f)*32.0);
        printf("$%02x%c ",(int)(freq*16.0  / 32000.0 * 4096.0)&0xff,f==2047?' ':',');
    }
    puts("");
    printf("snestabl_hi:\n.byte ");
    for (int f = 0; f < 2048; f++) {
        double freq = 4194304.0/((double)(2048-f)*32.0);
        printf("$%02x%c ",(int)(freq*16.0 / 32000.0 * 4096.0)>>8&0xff,f==2047?' ':',');
    }
    puts("");
    printf("noise_freq_table:\n.byte ");
    for (int f = 0; f < 256; f++) {
        uint8_t div = f&7;
        uint8_t shift = f>>4;
        double freq = 262144.0/((double)div*(double)(1<<shift));
        int fi = (int)freq;
        int ind = -1;
        int amt = -1;
        for (int i = 1; i < 32; i++) {
            if (ind == -1 || abs(fi-(int)(64000.0/(double)noise_sdsp[i])) < amt) {
                amt = abs(fi-(int)(64000.0/(double)noise_sdsp[i]));
                ind = i;
            } 
        }
        printf("$%02x%c ",ind,f==255?' ':',');
    }
    puts("");
}
