// SB+rlx_rlx+rlx_rlx
// Store Buffering (or Dekker's), with all four accesses relaxed
// Question: can the two reads both see 0 in the same execution?
int main() {
  atomic_int x=0; atomic_int y=0;
  {{{ { y.store(1,memory_order_relaxed);
        r1=x.load(memory_order_relaxed); }
  ||| { x.store(1,memory_order_relaxed);
        r2=y.load(memory_order_relaxed); }  }}}
  return 0;
}
