using System;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class Atom : Term
  {
    internal Atom(Runtime runtime, ErlNifTerm native) : base(runtime, native) {}
  }
}
