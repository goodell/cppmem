// a data race (dr)
int main() {
  int x = 2;
  int y;
  {{{  x = 3; 
  |||  y = (x==3);  
  }}};
  return 0; }
