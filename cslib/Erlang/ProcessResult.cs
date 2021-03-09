using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class ProcessResult : Term
  {
    internal ProcessResult(Runtime runtime, int handle) : base(runtime, handle) {}
  }
}
