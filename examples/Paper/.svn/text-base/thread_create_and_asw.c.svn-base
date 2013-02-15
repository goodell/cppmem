// thread-creation showing additional-synchronises-with (asw)
void foo(int* p) {*p = 3;}
int main() {
  int x = 2;
  int y;
  thread t1(foo, &x);
  y = 3;
  t1.join();
  return 0; }

