using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class Int : Term
  {
    internal Int(Runtime runtime, int handle) : base(runtime, handle) {}
  }
}
