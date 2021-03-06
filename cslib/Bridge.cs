using System;
using CsLib.Types;

namespace CsLib
{
    public class Bridge
    {
      private static Bridge instance;

      public static int Create(IntPtr arg, int argLength) {
        if(instance != null) {
          Console.WriteLine("Bridge already exists");
        } else {
          Console.WriteLine("Creating a bridge??");
          instance = new Bridge();
        }
        return 0;
      }
    }
}
