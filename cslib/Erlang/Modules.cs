using System;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class Modules : DynamicObject
  {
    
    public override bool TryGetMember(
        GetMemberBinder binder, out object result)
    {
      return new 
    }

    // If you try to set a value of a property that is
    // not defined in the class, this method is called.
    public override bool TrySetMember(
        SetMemberBinder binder, object value)
    {
        // Converting the property name to lowercase
        // so that property names become case-insensitive.
        dictionary[binder.Name.ToLower()] = value;

        // You can always add a value to a dictionary,
        // so this method always returns true.
        return true;
    }

  }

}
