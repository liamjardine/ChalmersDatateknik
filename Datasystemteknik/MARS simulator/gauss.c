/* reduce matrix A to upper triangular form */

void eliminate (double **A, int N) { int i, j, k; 
    
    /* loop over all diagonal (pivot) elements */ 
    for (k = 0; k < N; k++) { 
        /* for all elements in pivot row and right of pivot element */ 
        for (j = k + 1; j < N; j++) { 
            /* divide by pivot element */
            A[k][j] = A[k][j] / A[k][k];
            } 
        /* set pivot element to 1.0 */
        A[k][k] = 1.0; 
        /* for all elements below pivot row and right of pivot column */ 
        for (i = k + 1; i < N; i++) {
            for (j = k + 1; j < N; j++) {
                A[i][j] = A[i][j] - A[i][k] * A[k][j];
            }
            A[i][k] = 0.0; 
        }
    } /* end pivot loop */
} 
