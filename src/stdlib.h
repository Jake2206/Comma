#ifndef LIB

#define LIB

#include <stdio.h>
#include <stdlib.h>

#define BASE_MATRIX_SIZE 100

struct Matrix
{
    int rows;
    int columns;
    double **elements;
};

struct Array
{
    int length;
};

void print(char *str);
// struct Matrix *parseCSV(char *filepath);
void parseCSV(char *filepath);
double *reallocate_matrix(double *elements, int cur_size, int new_size);
struct Matrix *scalarMulti(double scalar, struct Matrix *matrix);
struct Matrix *scalarDiv(double scalar, struct Matrix *matrix);

/*
void outputCSV(struct Matrix *matrix, char *filepath);
struct Matrix *subtractMatrix(struct Matrix *base_matrix, struct Matrix *sub_matrix);
struct Matrix *addMatrix(struct Matrix *base_matrix, struct Matrix *add_matrix);
struct Matrix *dotProduct(struct Matrix *base_matrix, struct Matrix *dot_matrix);
struct Matrix *crossProduct(double base_vector[], double cross_vector[]);

*/
double retrieveElement(int row_index, int column_index, struct Matrix *matrix);

#endif