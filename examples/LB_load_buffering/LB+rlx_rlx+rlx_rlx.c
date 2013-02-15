// LB+rlx_rlx+rlx_rlx
// Load Buffering, with all four accesses relaxed
// Question: can the two reads both see 1 in the same execution?
int main() {
  atomic_int x=0; atomic_int y=0;
  {{{ { r1=x.load(memory_order_relaxed); 
        y.store(1,memory_order_relaxed); }
  ||| { r2=y.load(memory_order_relaxed);
        x.store(1,memory_order_relaxed); }  }}}
  return 0;
}
