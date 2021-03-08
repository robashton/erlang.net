using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class Tuple : Term
  {
    internal Tuple(Runtime runtime, int handle) : base(runtime, handle) {}
  }
}
