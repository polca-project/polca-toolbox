#include "traps.h"
#include <sys/time.h>
#include <stdlib.h>
#include <string.h> /*for size_t*/

#pragma polca type_size useless
int useless;

#pragma polca type_size cos1
#pragma polca type_size cos2
size_t typeFloat;

#pragma polca type_size image
#pragma polca type_size temp2d
size_t typeInt;


#pragma polca total_size temp2d
#pragma polca total_size cos1
#pragma polca total_size cos2
int size; 

#pragma polca total_size image
int sizeImage; 


void dct(int block[8][8],
         float temp2d[8][8],
         float cos1[8][8],
         float cos2[8][8]);

int main()
{
    float factor1;
    float factor2;
    float temp_cos;
    int image[128 * 128];
    int block[8][8];
    float cos1[8 * 8];
    float cos2[8 * 8];
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

    typeInt = sizeof(int);
    typeFloat = sizeof(float);

    size = 8 * 8;
    sizeImage = 128 * 128;
    
    for (i = 0; i < SAMPLES; i++)
    {
        gettimeofday(&tvalBefore, (void *) 0);
        factor1 = 2.0 * atan(1.0) / 8;
        factor2 = 0.0;
        for (m = 0; m < 8; ++m)
        {
            for (n = 0; n < 8; ++n)
            {
                temp_cos = cos(factor2 * (2 * n + 1)) / 8;
                #pragma polca init cos1
                cos1[m * 8 + n] = temp_cos;
                #pragma polca init cos2
                cos2[n * 8 + m] = temp_cos;
            }
            factor2 += factor1;
        }
        gettimeofday(&tvalAfter, (void *) 0);
        total = (tvalAfter.tv_sec - tvalBefore.tv_sec) * 1000000 + tvalAfter.tv_usec - tvalBefore.tv_usec;
        #pragma polca init image
        #pragma polca io 
        input_dsp(image, 16384, 1);
        gettimeofday(&tvalBefore, (void *) 0);
        #pragma polca kernel opencl
        #pragma polca def main_loop
        #pragma polca iteration_independent
        #pragma polca input cos1 
        #pragma polca input cos2
        #pragma polca input image 
        #pragma polca output temp2d
        #pragma polca output image 
        for (n = 0; n < 16 * 16; n++)
        {
            int polca_var_i_0;
            int polca_var_j_1;
            int polca_var_k_2;
            float polca_var_sum_3;
            for (polca_var_i_0 = 0; polca_var_i_0 < 8; polca_var_i_0++)
            {
                for (polca_var_j_1 = 0; polca_var_j_1 < 8; polca_var_j_1++)
                {
                    polca_var_sum_3 = 0.0;
                    for (polca_var_k_2 = 0; polca_var_k_2 < 8; polca_var_k_2++)
                    {
                        polca_var_sum_3 += image[(8 * (n / 16) + polca_var_i_0) * 128 + (8 * (n % 16) + polca_var_k_2)] * cos2[polca_var_k_2 * 8 + polca_var_j_1];
                    }
                    temp2d[polca_var_i_0 * 8 + polca_var_j_1] = polca_var_sum_3;
                    polca_var_sum_3 = 0.0;
                    for (polca_var_k_2 = 0; polca_var_k_2 < 8; polca_var_k_2++)
                    {
                        polca_var_sum_3 += cos1[polca_var_i_0 * 8 + polca_var_k_2] * temp2d[polca_var_k_2 * 8 + polca_var_j_1];
                    }
                    image[(8 * (n / 16) + polca_var_i_0) * 128 + (8 * (n % 16) + polca_var_j_1)] = polca_var_sum_3 < 0.0 ? (int) (polca_var_sum_3 - 0.5) : (int) (polca_var_sum_3 + 0.5);
                }
            }
        }
        for (n = 0; n < 16 * 16; n++)
        {
            result[n / 16][n % 16][0] = pack4in32(image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 0)],
                                                  image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 1)],
                                                  pack2in8(image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 2)] / 3,
                                                           image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 3)] / 3),
                                                  pack2in8(image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 4)] / 2,
                                                           image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 5)] / 2));
            result[n / 16][n % 16][1] = pack4in32(pack2in8(image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 6)],
                                                           image[(8 * (n / 16) + 0) * 128 + (8 * (n % 16) + 7)]),
                                                  image[(8 * (n / 16) + 1) * 128 + (8 * (n % 16) + 0)],
                                                  image[(8 * (n / 16) + 1) * 128 + (8 * (n % 16) + 1)],
                                                  pack2in8(image[(8 * (n / 16) + 1) * 128 + (8 * (n % 16) + 2)] / 2,
                                                           image[(8 * (n / 16) + 1) * 128 + (8 * (n % 16) + 3)]));
            result[n / 16][n % 16][2] = pack4in32(pack2in8(image[(8 * (n / 16) + 1) * 128 + (8 * (n % 16) + 4)],
                                                           image[(8 * (n / 16) + 1) * 128 + (8 * (n % 16) + 5)]),
                                                  pack2in8(image[(8 * (n / 16) + 2) * 128 + (8 * (n % 16) + 0)] / 3,
                                                           image[(8 * (n / 16) + 2) * 128 + (8 * (n % 16) + 1)] / 2),
                                                  pack2in8(image[(8 * (n / 16) + 2) * 128 + (8 * (n % 16) + 2)] / 2,
                                                           image[(8 * (n / 16) + 2) * 128 + (8 * (n % 16) + 3)]),
                                                  pack2in8(image[(8 * (n / 16) + 3) * 128 + (8 * (n % 16) + 0)] / 3,
                                                           image[(8 * (n / 16) + 3) * 128 + (8 * (n % 16) + 1)]));
            result[n / 16][n % 16][3] = pack4in32(pack2in8(image[(8 * (n / 16) + 3) * 128 + (8 * (n % 16) + 2)],
                                                           image[(8 * (n / 16) + 3) * 128 + (8 * (n % 16) + 3)]),
                                                  pack2in8(image[(8 * (n / 16) + 4) * 128 + (8 * (n % 16) + 0)] / 2,
                                                           image[(8 * (n / 16) + 4) * 128 + (8 * (n % 16) + 1)]),
                                                  pack2in8(image[(8 * (n / 16) + 5) * 128 + (8 * (n % 16) + 0)] / 2,
                                                           image[(8 * (n / 16) + 5) * 128 + (8 * (n % 16) + 1)]),
                                                  pack2in8(image[(8 * (n / 16) + 6) * 128 + (8 * (n % 16) + 0)],
                                                           image[(8 * (n / 16) + 7) * 128 + (8 * (n % 16) + 0)]));
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

