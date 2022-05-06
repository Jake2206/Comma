#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "stdlib.h"



void print(char *str) {
    printf("%s\n", str);
}



struct Matrix *parseCSV(char *filepath) {
    print(filepath);

    // Open and read from file
    FILE *fp;
    fp = fopen(filepath, "r");
    if (fp == NULL) {
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
    double *elements = (double *) malloc(BASE_MATRIX_SIZE * sizeof(double));
    int cur_elements_size = BASE_MATRIX_SIZE;
    int elements_index = 0;


    // Read a line from the file
    while ((read = getline(&line, &len, fp)) != -1) {
        // Increment the num of rows in matrix
	rows++;
        
	int column_index = 0;
        // Splits string by comma and parse double
	char *token = strtok(line, ",");
	while (token != NULL) {
            printf("token: %s\n", token);

	    // Parse string to double
            conv_double = strtod(token, &endptr);

	    // Checks if there is an error converting string to double
	    if (*endptr != 0 && !isspace((unsigned char)*endptr)) {
                printf("Unable to convert string to double: %s\n", token);
		exit(-1);
	    }

            column_index++;

	    // Set the element in the matrix elements array
            elements[elements_index++] = conv_double;
            
	    // Check to see if we need to allocate a bigger array
	    if (elements_index == cur_elements_size) {
                elements = reallocate_matrix(elements, cur_elements_size, 2*cur_elements_size);
		cur_elements_size = 2*cur_elements_size;
	    }

	    token = strtok(NULL, ",");
	}

        if (columns == 0) {
            columns = column_index;
	} else {
            // Make sure every row has the same number of columns
	    if (columns != column_index) {
                print("Matrix error: every row must have the same number of columns");
		exit(-1);
	    }
	}
    }

    fclose(fp);

    // Copy over matrix elements and deallocate malloced elements
    double final_elements[elements_index];
    int index;
    for (index = 0; index < elements_index; index++) {
        final_elements[index] = elements[index];
    } 

    free(elements);

    // Create Matrix instance
    struct Matrix *matrix = (struct Matrix *) (malloc(sizeof(struct Matrix)));
    matrix->rows = rows;
    matrix->columns = columns;
    matrix->elements = final_elements;

    return matrix;
}


double *reallocate_matrix(double *elements, int cur_size, int new_size) {
    // Double the size of array
    double *buf = (double *) malloc(new_size * sizeof(double));
    int index;
    for (index = 0; index < cur_size; index++) {
        buf[index] = elements[index];
    }

    free(elements);
    return buf;
}




/*
void outputCSV(struct Matrix *matrix, char *filepath) {
   
}

struct Matrix *scalarMulti(double scalar, struct Matrix *matrix) {
    return NULL;
}


struct Matrix *scalarDiv(double scalar, struct Matrix *matrix) {
    return NULL;
}


struct Matrix *subtractMatrix(struct Matrix *base_matrix, struct Matrix *sub_matrix) {
    return NULL;
}



struct Matrix *addMatrix(struct Matrix *base_matrix, struct Matrix *add_matrix) {
    return NULL;
}


struct Matrix *dotProduct(struct Matrix *base_matrix, struct Matrix *dot_matrix) {
    return NULL;
}


struct Matrix *crossProduct(double base_vector[], double cross_vector[]) {
    return NULL;
}


double retrieveElement(int row_index, int column_index, struct Matrix *matrix) {
    return 0.0;
}

*/


int main() {
    print("Hello world");
    
    parseCSV("test.csv");

    return 0;
}



