#include <sys/time.h>
#include "traps.h"
#include <stdlib.h>


void dct(int block[8][8],
         float temp2d[8][8],
         float cos1[8][8],
         float cos2[8][8]);

int main()
{
    float factor1;
    float factor2;
    float temp_cos;
    float cos1[8 * 8];
    float cos2[8 * 8];
    int image[128 * 128];
    int block[8][8];
    float temp2d[8 * 8];
    int result[16][16][4];
    int k;
    int l;
    int m;
    int n;
    int pack2in8();
    int pack4in32();
    struct timeval tvalBefore, tvalAfter;
    long total;
    int SAMPLES = 30;
    int i;
    int _ret_val_0;
    for (i = 0; i < SAMPLES; i++)
    {
        int z;
        gettimeofday(&tvalBefore, (void *) 0);
        factor1 = 2.0 * atan(1.0) / 8;
        factor2 = 0.0;
        for (m = 0; m < 8; ++m)
        {
            for (n = 0; n < 8; ++n)
            {
                temp_cos = cos(factor2 * (2 * n + 1)) / 8;
                cos1[m * 8 + n] = temp_cos;
                cos2[n * 8 + m] = temp_cos;
            }
            factor2 += factor1;
        }
        gettimeofday(&tvalAfter, (void *) 0);
        total = (tvalAfter.tv_sec - tvalBefore.tv_sec) * 1000000 + tvalAfter.tv_usec - tvalBefore.tv_usec;
        input_dsp(image, 16384, 1);
        gettimeofday(&tvalBefore, (void *) 0);
        for (z = 0; z < 16 * 16; z++)
        {
            int i;
            int j;
            float sum;
            for (i = 0; i < 8; i++)
            {
                for (j = 0; j < 8; j++)
                {
                    sum = 0.0;
                    for (k = 0; k < 8; k++)
                    {
                        sum += image[(8 * (z / 16) + i) * 128 + (8 * (z % 16) + k)] * cos2[k * 8 + j];
                    }
                    temp2d[i * 8 + j] = sum;
                    sum = 0.0;
                    for (k = 0; k < 8; k++)
                    {
                        sum += cos1[i * 8 + k] * temp2d[k * 8 + j];
                    }
                    image[(8 * (z / 16) + i) * 128 + (8 * (z % 16) + j)] = sum < 0.0 ? (int) (sum - 0.5) : (int) (sum + 0.5);
                }
            }
        }
        for (z = 0; z < 16 * 16; z++)
        {
            result[z / 16][z % 16][0] = pack4in32(image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 0)],
                                                  image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 1)],
                                                  pack2in8(image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 2)] / 3,
                                                           image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 3)] / 3),
                                                  pack2in8(image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 4)] / 2,
                                                           image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 5)] / 2));
            result[z / 16][z % 16][1] = pack4in32(pack2in8(image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 6)],
                                                           image[(8 * (z / 16) + 0) * 128 + (8 * (z % 16) + 7)]),
                                                  image[(8 * (z / 16) + 1) * 128 + (8 * (z % 16) + 0)],
                                                  image[(8 * (z / 16) + 1) * 128 + (8 * (z % 16) + 1)],
                                                  pack2in8(image[(8 * (z / 16) + 1) * 128 + (8 * (z % 16) + 2)] / 2,
                                                           image[(8 * (z / 16) + 1) * 128 + (8 * (z % 16) + 3)]));
            result[z / 16][z % 16][2] = pack4in32(pack2in8(image[(8 * (z / 16) + 1) * 128 + (8 * (z % 16) + 4)],
                                                           image[(8 * (z / 16) + 1) * 128 + (8 * (z % 16) + 5)]),
                                                  pack2in8(image[(8 * (z / 16) + 2) * 128 + (8 * (z % 16) + 0)] / 3,
                                                           image[(8 * (z / 16) + 2) * 128 + (8 * (z % 16) + 1)] / 2),
                                                  pack2in8(image[(8 * (z / 16) + 2) * 128 + (8 * (z % 16) + 2)] / 2,
                                                           image[(8 * (z / 16) + 2) * 128 + (8 * (z % 16) + 3)]),
                                                  pack2in8(image[(8 * (z / 16) + 3) * 128 + (8 * (z % 16) + 0)] / 3,
                                                           image[(8 * (z / 16) + 3) * 128 + (8 * (z % 16) + 1)]));
            result[z / 16][z % 16][3] = pack4in32(pack2in8(image[(8 * (z / 16) + 3) * 128 + (8 * (z % 16) + 2)],
                                                           image[(8 * (z / 16) + 3) * 128 + (8 * (z % 16) + 3)]),
                                                  pack2in8(image[(8 * (z / 16) + 4) * 128 + (8 * (z % 16) + 0)] / 2,
                                                           image[(8 * (z / 16) + 4) * 128 + (8 * (z % 16) + 1)]),
                                                  pack2in8(image[(8 * (z / 16) + 5) * 128 + (8 * (z % 16) + 0)] / 2,
                                                           image[(8 * (z / 16) + 5) * 128 + (8 * (z % 16) + 1)]),
                                                  pack2in8(image[(8 * (z / 16) + 6) * 128 + (8 * (z % 16) + 0)],
                                                           image[(8 * (z / 16) + 7) * 128 + (8 * (z % 16) + 0)]));
        }
        gettimeofday(&tvalAfter, (void *) 0);
        total += (tvalAfter.tv_sec - tvalBefore.tv_sec) * 1000000 + tvalAfter.tv_usec - tvalBefore.tv_usec;
        printf("it: %i, time: %ld microseconds\n", i, total);
    }
    output_dsp(result, 1024, 1);
    _ret_val_0 = 0;
    return _ret_val_0;
}

void dct(int block[8][8],
         float temp2d[8][8],
         float cos1[8][8],
         float cos2[8][8])
{
    int i;
    int j;
    int k;
    float sum;
    for (i = 0; i < 8; i++)
    {
        for (j = 0; j < 8; j++)
        {
            sum = 0.0;
            for (k = 0; k < 8; k++)
            {
                sum += block[i][k] * cos2[k][j];
            }
            temp2d[i][j] = sum;
        }
    }
    for (i = 0; i < 8; i++)
    {
        for (j = 0; j < 8; j++)
        {
            sum = 0.0;
            for (k = 0; k < 8; k++)
            {
                sum += cos1[i][k] * temp2d[k][j];
            }
            block[i][j] = sum < 0.0 ? (int) (sum - 0.5) : (int) (sum + 0.5);
        }
    }
    return;
}

int pack2in8(int a, int b)
{
    int _ret_val_0;
    a &= 15;
    b &= 15;
    _ret_val_0 = a << 4 | b;
    return _ret_val_0;
}

int pack4in32(int a, int b, int c, int d)
{
    int _ret_val_0;
    a &= 255;
    b &= 255;
    c &= 255;
    d &= 255;
    _ret_val_0 = a << 24 | b << 16 | c << 8 | d;
    return _ret_val_0;
}

