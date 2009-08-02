// for testing each function in the new C code

#include <stdio.h>
#include "utils.h"

int main(void)
{

   node* mypath=NULL;

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


	return 0;

}
