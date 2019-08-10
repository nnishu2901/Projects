#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#define SIZE 20

//use @ for identifier
//use # for power

typedef struct  {
  char non_terminal; //should be From A-Z and 0th non terminal is starting symbol
  int nop; //number of productions corresponding to non-terminal
  char **rhs; // rhs of production rules 
  int nof; //number of terminals + ^ in set of first 
  char f[100]; // set of first
  int noflw; // number of terminals + $ in set of follow
  char flw[100]; // set of follow
}production;

typedef struct {
  int nt,pn; //to refer to table entry production (pn)th production of (nt)th non terminal
}cell;

production *p;
cell **c;
char terminal[100];
int not=0,n; //not is number of terminals n is number of non terminals
char stack[150],input[50];
char *cip;
int top=-1;

int isTerminal(char c) {
  int i;
  for(i=0;i<not;i++) {
    if(c==terminal[i])
      return 1;
  }  
  return 0;
}

void first(int i) {
  int j,k,l,m,flag;
  for(j=0;j<p[i].nop;j++) {
    for(k=0;k<strlen(p[i].rhs[j]);k++) {
      if(isTerminal(p[i].rhs[j][k]) || p[i].rhs[j][k]=='^') {
        p[i].f[(p[i].nof)++]=p[i].rhs[j][k];
        break;
      }else {
        for(l=0;l<n;l++) {
          if(p[i].rhs[j][k]==p[l].non_terminal)
            break;
        }
        flag=0;
        if(p[l].nof==0) 
          first(l);
        for(m=0;m<p[l].nof;m++) {
          if(p[l].f[m]=='^') {
            flag=1;
            continue;
          }
          p[i].f[(p[i].nof)++]=p[l].f[m];
        }
        if(!flag) 
          break;
        if(flag && k==strlen(p[i].rhs[j])-1)
          p[i].f[(p[i].nof)++]='^';
      }
    }
  }    
}

int isThere(char c,char str[],int n) {
  if(n) {
    while(n!=-1) {
      if(c==str[n--])
        return 1;
    }
  }
  return 0;
}

void follow(int i) {
  int j,k,l,m,s,x,t,flag=0,whileflag=0;   //chose non-terminal
  for(j=0;j<n;j++) { //checking each production for that non terminal  
    for(k=0;k<p[j].nop;k++) {
      for(t=0;t<strlen(p[j].rhs[k]);t++) { // l is the position of non terminal which is matched with the non                                terminal whose follow is to be found
        whileflag=0;
        l=t;
        while(1) {
          if(p[i].non_terminal==p[j].rhs[k][l]) {
            if(isTerminal(p[j].rhs[k][l+1])) {
              p[i].flw[(p[i].noflw)++]=p[j].rhs[k][l+1];
              whileflag=1;
              break;
            }else if(l<strlen(p[j].rhs[k])-1){
            //  printf("here\n");
              for(m=0;m<n;m++) {
                if(p[j].rhs[k][l+1]==p[m].non_terminal) {
            //      printf("m %d\n",m);
                  flag=0;
                  for(s=0;s<p[m].nof;s++) {
                    if(!isThere(p[m].f[s],p[i].flw,p[i].noflw)) {
                      if(p[m].f[s]=='^') {
                        flag=1;
                        continue;
                      }
                      p[i].flw[(p[i].noflw)++]=p[m].f[s];
                    }
                  }
                  if(!flag) {
                    whileflag=1;
                    break;
                  }else if(l==strlen(p[j].rhs[k])-2) {
                    if(i!=j) {
                      if(p[j].noflw==0)
                        follow(j);
                    }
                    for(x=0;x<p[j].noflw;x++) {
                      if(!isThere(p[j].flw[x],p[i].flw,p[i].noflw))
                      //  printf("hi:%c ",p[j].flw[x]);
                        p[i].flw[(p[i].noflw)++]=p[j].flw[x];
                    }
                    whileflag=1;
                    break;
                  }else {
                    l++;  
                    m=0;              
                  }
                }
              }
            }else if(l==strlen(p[j].rhs[k])-1){
              if(i!=j) {
                if(p[j].noflw==0)
                  follow(j);
              }
              for(x=0;x<p[j].noflw;x++) {
                if(!isThere(p[j].flw[x],p[i].flw,p[i].noflw))
                  p[i].flw[(p[i].noflw)++]=p[j].flw[x];
              }
              whileflag=1;
              break;
            }
          }else {
            break;
          }
              if(whileflag==1)
                break;
        }
      }
    }
  }
}

void index_(char nt,char t,int *row,int *col) {
  int i;
  for(i=0;i<not;i++) {
    if(terminal[i]==t) {
      *col=i;
      break;
    }
  }
  for(i=0;i<n;i++) {
    if(p[i].non_terminal==nt) {
      *row=i;
      break;
    }
  }
}

void table() {
  int i,j,k,l,m,row,col,flag=0,forbreak=0;
  for(i=0;i<n;i++) {
    for(j=0;j<p[i].nop;j++) {
      for(k=0;k<strlen(p[i].rhs[j]);k++) {
        forbreak=0;
        if(isTerminal(p[i].rhs[j][k])) {
          index_(p[i].non_terminal,p[i].rhs[j][k],&row,&col);
          c[row][col].nt=i;
          c[row][col].pn=j;
          break;
        }else if(p[i].rhs[j][k]!='^') { //1st(kth) character is non terminal in production
          for(l=0;l<n;l++) {
            if(p[i].rhs[j][k]==p[l].non_terminal) //l is the non terminal at kth position of jth                                 production ith non terminal
              break;
          }
          flag=0;
        //  printf("%c %d\n",p[i].non_terminal,p[l].nof);
          for(m=0;m<p[l].nof;m++) {
          //  printf("%c %c\n",p[i].non_terminal,p[l].f[m]);
            if(p[l].f[m]=='^') {
              flag=1;  
              continue;
            }
            index_(p[i].non_terminal,p[l].f[m],&row,&col);
            c[row][col].nt=i;
            c[row][col].pn=j;          
          }
          if(!flag) {
            forbreak=1;
          //  break;  
          }    
        }
        if((k==strlen(p[i].rhs[j])-1 && flag) ||(p[i].rhs[j][k]=='^')) { //production of type T -> ^ || all the non terminals  have ^ in their set first and follow of T is to be included
          for(l=0;l<p[i].noflw;l++) {
            index_(p[i].non_terminal,p[i].flw[l],&row,&col);
            c[row][col].nt=i;
            c[row][col].pn=j;
          }
        }
        if(forbreak==1)
          break;
      }
    }
  }
}

int main(void) {
  int i,j,k,l,m,len,flag=0; 
  printf("enter number of non-terminals : ");
  scanf("%d",&n);
  p=(production *)malloc(n*sizeof(production));
  printf("\nenter LL(1) grammar in precedence from starting symbol\n");
  for(i=0;i<n;i++) {
    p[i].nof=0;
    p[i].noflw=0;
    printf("\n--------------------------------------------------------------------------------------------\n");
    printf("\nenter non-terminal : ");
    do {
      scanf("%c",&p[i].non_terminal);
    }while(p[i].non_terminal=='\n');
    printf("\nenter NUMBER of rhs PRODUCTIONS corresponding to given non-terminal : ");
    scanf("%d",&p[i].nop);
    p[i].rhs=(char **)malloc(p[i].nop*sizeof(char *));
    for(j=0;j<p[i].nop;j++) {  
      p[i].rhs[j]=(char *)malloc(SIZE*sizeof(char));
      printf("\nenter rhs production %d of non-terminal %c : ",(j+1),p[i].non_terminal);
      do {
        scanf("%s",p[i].rhs[j]);
      }while(p[i].rhs[j][0]=='\n');
      if(p[i].rhs[j][strlen(p[i].rhs[j])-1]=='\n')
        p[i].rhs[j][strlen(p[i].rhs[j])-1]='\0';
//      printf("%s",p[i].rhs[j]);
      for(k=0;k<strlen(p[i].rhs[j]);k++) {
        if(!(p[i].rhs[j][k]>='A' && p[i].rhs[j][k]<='Z') && p[i].rhs[j][k]!='^') {
          for(l=0;l<not;l++) {
            if(terminal[l]==p[i].rhs[j][k]) {
              break;
            }
          }
          if(l==not) {
            terminal[not++]=p[i].rhs[j][k];
          }
        }    
      }
    }
  }
  c=(cell **)calloc(n,sizeof(cell *)); // error is represented as -1,-1 in the table 
  for(i=0;i<n;i++)
    c[i]=(cell *)calloc((not+1),sizeof(cell)); // + 1 for $
  for(i=0;i<n;i++) {
    for(j=0;j<not+1;j++) {
      c[i][j].nt=-1;
      c[i][j].pn=-1;
    }
  }
  printf("\n--------------------------------------TERMINALS-----------------------------------------\n");
  for(i=0;i<not;i++)
    printf("%c ",terminal[i]);
  printf("\n");
  terminal[not++]='$';
  printf("\n--------------------------------------PRODUCTIONS-----------------------------------------\n");  
  for(i=0;i<n;i++) {
    for(j=0;j<p[i].nop;j++)
      printf("%c->%s\n",p[i].non_terminal,p[i].rhs[j]);
  }

  for(i=0;i<n;i++) {
    if(p[i].nof==0)
      first(i);
  }
  printf("\n--------------------------------------FIRST-----------------------------------------\n");
  for(i=0;i<n;i++) {
    printf("first(%c) = {",p[i].non_terminal);
    for(j=0;j<p[i].nof;j++) {
      printf("%c",p[i].f[j]);
      if(j!=p[i].nof-1)
        printf(",");
    }
    printf("}\n");
  }
  p[0].noflw=1;
  p[0].flw[0]='$';
  printf("\n--------------------------------------FOLLOW-----------------------------------------\n");
  follow(0);
  for(i=1;i<n;i++) {
    if(p[i].noflw==0)  
      follow(i);
  }
  for(i=0;i<n;i++) {
    printf("follow(%c) = {",p[i].non_terminal);
    for(j=0;j<p[i].noflw;j++) {
      printf("%c",p[i].flw[j]);
      if(j!=p[i].noflw-1)
        printf(",");
    }
    printf("}\n");
  }
  table();
  printf("\n---------------------------------PARSING TABLE-----------------------------------------\n\n    |   ");
  for(i=0;i<not;i++)
    printf("%c\t",terminal[i]);
  printf("\n");
  printf("____|_______________________________________________________________________\n");
  for(i=0;i<n;i++) {
    printf("%c   |\t",p[i].non_terminal);
    for(j=0;j<not;j++) {
      if(c[i][j].nt==-1 && c[i][j].pn==-1) {
        printf("error\t");
      }
      else
        printf("%c->%s\t",p[c[i][j].nt].non_terminal,p[c[i][j].nt].rhs[c[i][j].pn]);
    }
    printf("\n");
  }
  int test;
  printf("enter number of strings to be verified : ");
  scanf("%d",&test);
  while(test--) {
    top=-1;
    stack[++top]='$';
    stack[++top]=p[0].non_terminal;
    printf("\nenter INPUT STRING to be checked corresponding to LL(1) grammar : ");
    do {
      scanf("%s",input);
    }while(input[0]=='\n');
    if(strlen(input)-1=='\n')
      input[strlen(input)-1]='\0';
    flag=0;
    for(i=0;i<strlen(input);i++)
      if(input[i]=='$') {
        flag=1;
        printf("\nInvalid input : %s\n",input);
        break;
      }
    if(flag)
      continue;
    input[strlen(input)+1]='\0';
    input[strlen(input)]='$';
    printf("\ninput : %s\n\n",input);
    cip=&input[0];
    printf("STACK\t\tINPUT\t\tACTION\n");
    printf("________________________________________________________________________________________\n");  
    while(1) {
      for(l=0;l<=top;l++)
        printf("%c",stack[l]);
      printf("\t\t%s\t\t",cip);
      if(*cip=='$' && stack[top]==*cip) {
        printf("ACCEPTED\n\nGiven input follows given LL(1) grammar\n");
        break;
      }else if(isTerminal(*cip) && *cip!='$' && stack[top]==*cip) {
        cip++;
        top--;
        printf("Match\n");
      }else if(!isTerminal(*cip)) {
        printf("ERROR\n\nCharacter '%c' unexpected in input string\n",*cip);
        break;
      }else if(!isTerminal(stack[top])) {
        index_(stack[top],*cip,&i,&j);
        k=c[i][j].nt;
        l=c[i][j].pn;
        if(k==-1 || l==-1) {
          printf("ERROR\n\nInput string doesn't match given LL(1) grammar\n");
          break;
        };
        printf("Production : %c->%s\n",p[k].non_terminal,p[k].rhs[l]);
        for(m=strlen(p[k].rhs[l])-1;m>=0;m--) {
          if(p[k].rhs[l][m]!='^')
            stack[top++]=p[k].rhs[l][m];
        }
        top--;
      }else {
        printf("ERROR\n\nUnexpected appended string : '%s'\n",cip);
        break;
      }
    }
    printf("\n##########################################################################\n");
  }
  return 0;
}
