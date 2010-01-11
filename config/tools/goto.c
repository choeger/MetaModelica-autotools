/* goto.c */
#include <stdio.h>

char *doit(unsigned n)
{
    static const void ** labels[3] = {
	&&label0, &&label1, &&label2
    };
    goto *labels[n];
  label0:
    return "label0";
  label1:
    return "label1";
  label2:
    return "label2";
}

int main(void)
{
    printf("%s\n", doit(2));
    return 0;
}
