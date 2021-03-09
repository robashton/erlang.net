using System;
using CsLib;

namespace CsLib.Erlang 
{
  public sealed class ProcessResult : Term
  {
    internal ProcessResult(Runtime runtime, Int64 handle) : base(runtime, handle) {}
  }
}
