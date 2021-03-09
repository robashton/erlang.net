using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class Int : Term
  {
    internal Int(Runtime runtime, Int64 handle) : base(runtime, handle) {}
  }
}
