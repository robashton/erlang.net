using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface ITerm
    {
      Int64 Handle();
      bool HasValue 
      {
        get;
      }
    }
}
