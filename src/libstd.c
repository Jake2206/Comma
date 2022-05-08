#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "libstd.h"

void print(char *str)
{
    printf("%s\n", str);
}


void printHello() {
    printf("Hello\n");
}


struct Matrix *parseCSV(char *filepath)
{
    // Open and read from file
    FILE *fp;
    fp = fopen(filepath, "r");
    if (fp == NULL)
    {
        print("No such file exists: ");
        print(filepath);
        // Handle error case
        exit(-1);
    }

    // Initialize file read vars
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    // Initialize string-to-double vars
    char *endptr;
    double conv_double;

    // Num of Rows and Num of Columns this matrix will have
    int rows = 0;
    int columns = 0;
    double *elements = (double *)malloc(BASE_MATRIX_SIZE * sizeof(double));
    int cur_elements_size = BASE_MATRIX_SIZE;
    int elements_index = 0;

    // Read a line from the file
    while ((read = getline(&line, &len, fp)) != -1)
    {
        // Increment the num of rows in matrix
        rows++;

        int column_index = 0;
        // Splits string by comma and parse double
        char *token = strtok(line, ",");
        while (token != NULL)
        {
            // Parse string to double
            conv_double = strtod(token, &endptr);

            // Checks if there is an error converting string to double
            if (*endptr != 0 && !isspace((unsigned char)*endptr))
            {
                printf("Unable to convert string to double: %s\n", token);
                exit(-1);
            }

            column_index++;

            // Set the element in the matrix elements array
            elements[elements_index++] = conv_double;

            // Check to see if we need to allocate a bigger array
            if (elements_index == cur_elements_size)
            {
                elements = reallocate_matrix(elements, cur_elements_size, 2 * cur_elements_size);
                cur_elements_size = 2 * cur_elements_size;
            }

            token = strtok(NULL, ",");
        }

        if (columns == 0)
        {
            columns = column_index;
        }
        else
        {
            // Make sure every row has the same number of columns
            if (columns != column_index)
            {
                print("Matrix error: every row must have the same number of columns");
                exit(-1);
            }
        }
    }

    fclose(fp);

    // Copy over matrix elements and deallocate malloced elements
    struct Matrix *matrix = createMatrix(rows, columns);

    for (int row_index = 0; row_index < rows; row_index++)
    {
        for (int column_index = 0; column_index < columns; column_index++)
        {
            matrix->elements[row_index][column_index] = elements[row_index * columns + column_index];
        }
    }

    free(elements);

    return matrix;
}

double *reallocate_matrix(double *elements, int cur_size, int new_size)
{
    // Double the size of array
    double *buf = (double *)malloc(new_size * sizeof(double));
    int index;
    for (index = 0; index < cur_size; index++)
    {
        buf[index] = elements[index];
    }

    free(elements);
    return buf;
}


void outputCSV(struct Matrix *matrix, char *filepath) {
    
    FILE *fp;
    fp = fopen(filepath, "w+");

    for (int row = 0; row < matrix->rows; row++) {
        for (int col = 0; col < matrix->columns; col++) {
            fprintf(fp, "%f", matrix->elements[row][col]);
            if (col != ((matrix->columns) - 1)) {
                fprintf(fp, ",");
	    }
	}
	fprintf(fp, "\n");
    }

    fclose(fp);
}


struct Matrix *createMatrix(int rows, int columns) {
   double **elements = malloc(rows * sizeof(double *));
   for (int i = 0; i < rows; i++) {
       elements[i] = malloc(columns * sizeof(double));
   }
   struct Matrix *matrix = (struct Matrix *) malloc(sizeof(struct Matrix));
   matrix->rows = rows;
   matrix->columns = columns;
   matrix->elements = elements;
   return matrix;
}



struct Matrix *subtractMatrix(struct Matrix *base_matrix, struct Matrix *sub_matrix) {
    
    if (base_matrix->rows != sub_matrix->rows || base_matrix->columns != sub_matrix->columns) {
        print("Invalid matrix subtraction: matrices must be of the same dimensions");
	exit(-1);
    }
    
    struct Matrix *calcMatrix = createMatrix(base_matrix->rows, base_matrix->columns);

    for (int row = 0; row < base_matrix->rows; row++) {
        for (int col = 0; col < base_matrix->columns; col++) {
            calcMatrix->elements[row][col] = base_matrix->elements[row][col] - sub_matrix->elements[row][col];
	}
    }
	
    return calcMatrix;
}



struct Matrix *addMatrix(struct Matrix *base_matrix, struct Matrix *add_matrix) {
    if (base_matrix->rows != add_matrix->rows || base_matrix->columns != add_matrix->columns) {
        print("Invalid matrix addition: matrices must be of the same dimensions");
	exit(-1);
    }

    struct Matrix *calcMatrix = createMatrix(base_matrix->rows, base_matrix->columns);

    for (int row = 0; row < base_matrix->rows; row++) {
        for (int col = 0; col < base_matrix->columns; col++) {
            calcMatrix->elements[row][col] = base_matrix->elements[row][col] + add_matrix->elements[row][col];
	}
    }
	
    return calcMatrix;
}



struct Matrix *dotProduct(struct Matrix *base_matrix, struct Matrix *dot_matrix) {
    if (base_matrix->columns != dot_matrix->rows) {
       print("Invalid matrix dot product: first matrix columns must equal second matrix rows");
       exit(-1);
    }

    struct Matrix *calcMatrix = createMatrix(base_matrix->rows, dot_matrix->columns);
    
    for (int row = 0; row < base_matrix->rows; row++) {
        for (int col = 0; col < dot_matrix->columns; col++) {
            int sum = 0;
	    for (int i = 0; i < base_matrix->columns; i++) {
                sum = sum + (base_matrix->elements[row][i] * dot_matrix->elements[i][col]);
	    }
	    calcMatrix->elements[row][col] = sum;
	}
    }


    return calcMatrix;
}


struct Matrix *crossProduct(struct Array *base_vector, struct Array *cross_vector) {
    if (base_vector->length != 3 || cross_vector->length != 3) {
        print("Invalid vector cross product: each vector must have dimensions of 3");
	exit(-1);
    }

    struct Matrix *calcMatrix = createMatrix(1,3);

    double s0 = (base_vector->elements[1] * cross_vector->elements[2]) - (base_vector->elements[2] * cross_vector->elements[1]);
    double s1 = (base_vector->elements[2] * cross_vector->elements[0]) - (base_vector->elements[0] * cross_vector->elements[2]);
    double s2 = (base_vector->elements[0] * cross_vector->elements[1]) - (base_vector->elements[1] * cross_vector->elements[0]);

    calcMatrix->elements[0][0] = s0;
    calcMatrix->elements[0][1] = s1;
    calcMatrix->elements[0][2] = s2;

    return calcMatrix;
}

struct Matrix *scalarMulti(double scalar, struct Matrix *matrix)
{
    for (int i = 0; i < matrix->rows; i++)
    {
        for (int j = 0; j < matrix->columns; j++)
        {
            matrix->elements[i][j] *= scalar;
        }
    }
    return matrix;
}

struct Matrix *scalarDiv(double scalar, struct Matrix *matrix)
{
    for (int i = 0; i < matrix->rows; i++)
    {
        for (int j = 0; j < matrix->columns; j++)
        {
            matrix->elements[i][j] /= scalar;
        }
    }
    return matrix;
}

double retrieveElement(int row_index, int column_index, struct Matrix *matrix)
{
    if (column_index >= matrix->columns || column_index < 0)
    {
        print("Matrix error: column index out of bounds\n");
        return -1.0;
    }
    else if (row_index >= matrix->rows || row_index < 0)
    {
        print("Matrix error: row index out of bounds\n");
        return -1.0;
    }
    return matrix->elements[row_index][column_index];
}


int main()
{
    printHello();
    //struct Matrix *matrix = parseCSV("test.csv");
    //struct Matrix *dotMatrix = parseCSV("test2.csv");

    //struct Matrix *calcMatrix = dotProduct(matrix, dotMatrix);
    
    //outputCSV(calcMatrix, "testOutput.csv");

    struct Array *v1 = malloc(sizeof(struct Array));
    struct Array *v2 = malloc(sizeof(struct Array));

    v1->length = 3;
    v1->elements = malloc(3 * sizeof(double));
    v2->length = 3;
    v2->elements = malloc(3 * sizeof(double)); 

    v1->elements[0] = 1;
    v1->elements[1] = 3;
    v1->elements[2] = 4;
    v2->elements[0] = 2;
    v2->elements[1] = 7;
    v2->elements[2] = -5;


    struct Matrix *calcMatrix = crossProduct(v1, v2);
    outputCSV(calcMatrix, "testOutput.csv");

    /*
    print("Hello world");

    struct Matrix *matrix = parseCSV("test.csv");
    int matrixLength = matrix->columns * matrix->rows;
    printf("%lf\n", retrieveElement(1, 2, matrix));
    printf("%lf\n", retrieveElement(1, 3, matrix));
    printf("%lf\n", retrieveElement(1, 4, matrix));
    scalarMulti(3.25, matrix);
    printf("----------\n");
    printf("%lf\n", retrieveElement(1, 2, matrix));
    printf("%lf\n", retrieveElement(1, 3, matrix));
    printf("%lf\n", retrieveElement(1, 4, matrix));
    scalarDiv(3.25, matrix);
    printf("----------\n");
    printf("%lf\n", retrieveElement(1, 2, matrix));
    printf("%lf\n", retrieveElement(1, 3, matrix));
    printf("%lf\n", retrieveElement(1, 4, matrix));
    return 0;
    */
} 

