// for testing each function in the new C code

#include <stdio.h>
#include "utils.h"

int main(void)
{

   node* mypath=NULL;

	// test AppendNode and Push

	double data[2];

	data[0]=3;
	data[1]=3;
   AppendNode(&mypath, data);

	data[0]=4;
	data[1]=4;
   AppendNode(&mypath, data);

	data[0]=1;
	data[1]=1;
   Push(&mypath, data);
	
	data[0]=2;
	data[1]=2;
   Push(&mypath, data);

	node* current=mypath;

	int i=0;

	while (current != NULL) {
		i++;
   	printf("%d:(%f,%f)\n",i,current->data[0],current->data[1]);
   	current = current->next;
	}


	// test CopyList
	printf("------\n");

   node* copyoflist=NULL;

	copyoflist=CopyList(mypath);
	current=copyoflist;

	i=0;

	while (current != NULL) {
		i++;
   	printf("%d:(%f,%f)\n",i,current->data[0],current->data[1]);
   	current = current->next;
	}



   // test intpoint

	printf("------\n");
   
   double p1[2] = {1,1};
   double p2[2] = {-1,-1};
   double edge[2][2];
   double ip[2];

   edge[0][0]=-1;
   edge[0][1]=1;
   edge[1][0]=1;
   edge[1][1]=-1;

   intpoint(p1,p2,edge,ip);
   printf("ip= %f, %f\n",ip[0],ip[1]);


	return 0;
}
