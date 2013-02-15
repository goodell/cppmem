// WRC
// the question is whether the final read is required to see 1
// With two release/acquire pairs, it is
int main() {
  atomic_int x = 0;
  atomic_int y = 0;

  {{{ x.store(1,mo_release);
  ||| { r1=x.load(mo_acquire).readsvalue(1);
        y.store(1,mo_release); }
  ||| { r2=y.load(mo_acquire).readsvalue(1);
        r3=x.load(mo_relaxed); }
  }}}
  return 0;
}
