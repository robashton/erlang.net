using System;
using CsLib;


namespace CsLib.Erlang 
{
  public sealed class PointerResource : Term
  {
    internal PointerResource(Runtime runtime, int handle) : base(runtime, handle) {}
  }
}
