using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class Tuple : Term
  {
    internal Tuple(Runtime runtime, Int64 handle) : base(runtime, handle) {}
  }
}
