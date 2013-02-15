// LB+sc_sc+sc_sc
// Load Buffering, with all four accesses sequentially consistent atomics
// Question: can the two reads both see 1 in the same execution?
int main() {
  atomic_int x=0; atomic_int y=0;
  {{{ { r1=x.load(memory_order_seq_cst); 
        y.store(1,memory_order_seq_cst); }
  ||| { r2=y.load(memory_order_seq_cst);
        x.store(1,memory_order_seq_cst); }  }}}
  return 0;
}
