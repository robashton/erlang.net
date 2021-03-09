using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class Atom : Term
  {
    internal Atom(Runtime runtime, Int64 handle) : base(runtime, handle) {}
  }
}
