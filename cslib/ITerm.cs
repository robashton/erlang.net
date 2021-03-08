using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface ITerm
    {
      int Handle();
      bool HasValue 
      {
        get;
      }
    }
}
