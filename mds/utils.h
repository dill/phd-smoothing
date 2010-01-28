/* First hash at re-writing the utils.R script in C */
// Copyright 2009 David Lawrence Miller

#include <math.h>
#include <stdlib.h>
#include <stdio.h>


// definition of linked list
typedef struct node
{
   double data[2];  // space for x and y
   struct node* next; // pointer to next item
   struct node* prev; // pointer to previous item
} node;

// some prototypes
void twosort(double *);
void itwosort(int *);
int online(double[2],double[2][2]);
int facing(double p1[2], double p2[2] , int nbnd, double **);
void intpoint(double[2], double[2],double[2][2],double[2]);
void do_intersect(double[2], double[2], int nbnd, double **,int *);
double minarr(int narr, double *);
double maxarr(int narr, double *);
int compare_doubles (const void *a, const void *b);
int crapfind(int narr, double *, double);
int iarrsum(int narr, int *);
double hull_length(node**);
void sp_do_intersect(double[2], double[2], int nbnd, double **,int *);
int first_ips(double[2], double[2], int nbnd, double **, 
               double[2], double[2], int[2]);
void Push(node**, double[2]);
void AppendNode(node**, double[2]);
void CopyList(node*, node**);
int Length(node*);
void FreeList(node**);
void PrintPath(node*);
void DelTopBot(node*, node**);

// for in_out in separate file
void in_out(double *, double *, double *,double *,double *,int *, int *, int * );


