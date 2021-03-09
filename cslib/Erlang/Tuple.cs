using System;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class Tuple : Term
  {
    internal Tuple(Runtime runtime, ErlNifTerm native) : base(runtime, native) {}
  }
}
