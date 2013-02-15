// LB+acq_rel+acq_rel
// Load Buffering, with acquire/release pairs
// Question: can the two reads both see 1 in the same execution?
int main() {
  atomic_int x=0; atomic_int y=0;
  {{{ { r1=x.load(memory_order_acquire); 
        y.store(1,memory_order_release); }
  ||| { r2=y.load(memory_order_acquire);
        x.store(1,memory_order_release); }  }}}
  return 0;
}
