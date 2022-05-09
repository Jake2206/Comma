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
    double *elements;
};

struct Matrix *initMatrix(int rows, int columns, double **elements);
void print(char *str);
void printMatrix(struct Matrix *matrix);
void printHello();
struct Matrix *createMatrix(int rows, int columns);
struct Matrix *parseCSV(char *filepath);
double *reallocate_matrix(double *elements, int cur_size, int new_size);
struct Matrix *scalarMulti(double scalar, struct Matrix *matrix);
struct Matrix *scalarDiv(double scalar, struct Matrix *matrix);
void outputCSV(struct Matrix *matrix, char *filepath);
struct Matrix *subtractMatrix(struct Matrix *base_matrix, struct Matrix *sub_matrix);
struct Matrix *addMatrix(struct Matrix *base_matrix, struct Matrix *add_matrix);


struct Matrix *dotProduct(struct Matrix *base_matrix, struct Matrix *dot_matrix);


struct Matrix *crossProduct(struct Array *base_vector, struct Array *cross_vector);


double retrieveElement(int row_index, int column_index, struct Matrix *matrix);

#endif
