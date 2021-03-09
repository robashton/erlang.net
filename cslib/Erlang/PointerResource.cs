using System;
using CsLib;


namespace CsLib.Erlang 
{
  public sealed class PointerResource : Term
  {
    internal PointerResource(Runtime runtime, Int64 handle) : base(runtime, handle) {}
  }
}
