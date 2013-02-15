// MP+rlx_rlx+rlx_rlx+obs
// Message Passing, with relaxed data and synchronisation.
//  This is race-free, as all accesses are atomic, but the
//  relaxed atomics mean that the second and third threads
//  can see the writes to x and y in different orders.
int main() {
  atomic_int x = 0; atomic_int y = 0;
  {{{  { x.store(1, mo_relaxed);
         y.store(1, mo_relaxed); }
  ||| { r1 = x.load(mo_relaxed).readsvalue(1);
        r2 = y.load(mo_relaxed).readsvalue(0); }
  ||| { r3 = y.load(mo_relaxed).readsvalue(1);
        r4 = x.load(mo_relaxed).readsvalue(0); }
  }}};
  return 0; }
