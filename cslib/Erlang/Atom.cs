using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class Atom : Term
  {
    internal Atom(Runtime runtime, int handle) : base(runtime, handle) {}
  }
}
