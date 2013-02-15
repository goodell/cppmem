// contrasting with data_race.c, this 
// shows a concurrent use of sc_atomic that does
// not have a data race
int main() {
  atomic_int x = 2; 
  int y = 0;
  {{{ x.store(3);
  ||| y = ((x.load())==3); 
  }}};
  return 0; }
