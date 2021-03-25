using System;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;

using Erlang.Internal;

namespace Erlang
{
  public delegate ErlNifTerm ErlangCallback(ErlNifTerm args);

  public unsafe static class Erl
  {
    private static ThreadLocal<ErlNifEnv> env = new();

    public static dynamic Modules
    { 
      get { return new Modules(); }
    }

    internal static ErlNifEnv Env {
      get { return env.Value; }
    }

    internal static ErlNifTerm WithEnv(ErlNifEnv e, Func<ErlNifTerm> cb) {
      env.Value = e;
      try {
        return cb();
      } catch (TermException ex) {
        return Erl.ExportAuto(Tuple.Create(new Atom("error"), ex.Term));
      } catch (Exception ex) {
        return Erl.ExportAuto(Tuple.Create(new Atom("error"), ex.Message));
      } finally {
        env.Value = ErlNifEnv.Zero;
      }
    }

    public static ErlNifTerm CallErlangFn(String module, String fn, ErlNifTerm[] args) {
      return Imports.erldotnet_call_erlang_fn(Env, 
          MakeTuple3(MakeAtom(module), MakeAtom(fn), MakeList(args)));
    }

    public static ErlNifTerm WriteDebug(String value) {
      return Imports.erldotnet_write_debug(Env, new (value));
    }

    public static ErlNifTerm MakeAtom(String value) {
      return Imports.erldotnet_make_atom(Env, new (value));
    }

    public static ErlNifTerm MakeInt(Int32 value) {
      return Imports.erldotnet_make_int(Env, value);
    }

    public static ErlNifTerm MakeInt64(Int64 value) {
      return Imports.erldotnet_make_int64(Env, value);
    }

    public static ErlNifTerm MakeString(String value) {
      return Imports.erldotnet_make_string(Env, new (value));
    }

    public static ErlNifTerm MakeBinary(byte[] value) {
      return Imports.erldotnet_make_binary(Env, value.Length, value);
    }

    public static ErlNifTerm MakeTuple(ErlNifTerm[] terms) {
      return Imports.erldotnet_make_tuple(Env, (uint)terms.Length, terms);
    }

    public static ErlNifTerm MakeTuple2(ErlNifTerm a, ErlNifTerm b) {
      return Imports.erldotnet_make_tuple2(Env, a, b);
    }

    public static ErlNifTerm MakeTuple3(ErlNifTerm a, ErlNifTerm b, ErlNifTerm c) {
      return Imports.erldotnet_make_tuple3(Env, a, b, c);
    }

    public static ErlNifTerm MakeList(ErlNifTerm value) {
      return Imports.erldotnet_make_list1(Env, value);
    }

    public static ErlNifTerm MakeList(ErlNifTerm[] value) {
      return Imports.erldotnet_make_listn(Env, (uint)value.Length, value);
    }

    public static ErlNifTerm MakeMap(ErlNifTerm[] keys, ErlNifTerm[] values) {
      return Imports.erldotnet_make_map(Env, (uint)keys.Length, keys, values);
    }

    public static ErlNifTerm MakePid(Pid value) {
      return Imports.erldotnet_make_pid(Env, value);
    }

    public static ErlNifTerm MakeObjectReference(Object obj) {
      var handle = GCHandle.Alloc(obj);
      var ptr = GCHandle.ToIntPtr(handle);
      return Imports.erldotnet_make_pointer_resource(Env, (delegate* <IntPtr, void>)Marshal.GetFunctionPointerForDelegate(ReturnObjectReferenceInstance), ptr);
    }

    public static Object GetObjectReference(ErlNifTerm c) {
      var ptr = Imports.erldotnet_unpack_pointer_resource(Env, c);
      var handle = GCHandle.FromIntPtr(ptr);
      return handle.Target;
    }

    private delegate void ReturnObjectReference(IntPtr handle);
    private static ReturnObjectReference ReturnObjectReferenceInstance = (ptr) => GCHandle.FromIntPtr(ptr).Free();

    public static void Send(Pid target, ErlNifTerm term) {
      Imports.erldotnet_send(Env, target, term);
    }

    public static void Send(Pid target, Object o) {
      Imports.erldotnet_send(Env, target, ExportAuto(o));
    }

    public static T Coerce<T>(ErlNifTerm term) {
      object result = Coerce(term, typeof(T));
      if(result == null) {
        return default(T);
      }
      return (T)result;
    }
    
    public static Object ExtractAuto(ErlNifTerm term) {
      Type t = DeriveType(term);
      return Coerce(term, t);
    }

    public static ErlNifTerm ExportAuto(Object obj) {
      if(obj == null) { return MakeAtom("undefined"); }
      Type t = obj.GetType();

      if(t == typeof(Atom)) { return MakeAtom((Atom)obj); }
      if(t == typeof(Int32)) { return MakeInt((Int32)obj); }
      if(t == typeof(Int64)) { return MakeInt64((Int64)obj); }
      if(t == typeof(String)) { return MakeString((String)obj); }
      if(t == typeof(byte[])) { return MakeBinary((byte[])obj); }
      if(t == typeof(Pid)) { return MakePid((Pid)obj); }
      if(t == typeof(ErlNifTerm)) { return (ErlNifTerm)obj; }
      if(t == typeof(ErlangCallback)) { return MakeObjectReference(obj); }
      if(t == typeof(Object[])) { return MakeList(((Object[])(obj)).Select(o => ExportAuto(o)).ToArray()); }

      if(t.IsTuple()) {
        ITuple tuple = (ITuple)obj;
        var terms = Enumerable.Range(0, tuple.Length)
              .Select(i => ExportAuto(tuple[i]))
              .ToArray();
        return MakeTuple(terms);
      }

      if(t.IsRecord()) {

        var nameTerms = t.GetProperties()
          .Select(x => MakeAtom(x.Name.ToLower()))
          .ToArray();

        var valueTerms = t.GetProperties()
          .Select(x => ExportAuto(x.GetValue(obj)))
          .ToArray();

        return MakeMap(nameTerms, valueTerms);
      }

      return ErlNifTerm.Zero;

      // TODO: Float/Double
      // TODO: Binary

    }

    public static Type DeriveType(ErlNifTerm term)  {
      if(Imports.erldotnet_is_tuple(Env, term)) {
        var length = Imports.erldotnet_tuple_length(Env, term);
        var typeArguments = Enumerable.Range(0, length)
          .Select(i =>  DeriveType(Imports.erldotnet_tuple_element(Env, i, term)))
          .ToArray();

        var method =  typeof(System.Tuple).GetMethods()
                         .FirstOrDefault(method => method.Name == "Create" && method.GetParameters().Length == typeArguments.Length)
                         .MakeGenericMethod(typeArguments);

        return method.ReturnType;
      }

      // TF this is only an elaborate April Fools
      // and I don't have to actually support this code in any way
      if(Imports.erldotnet_is_atom(Env, term)) {
        return typeof(Atom);
      }
      if(Imports.erldotnet_is_double(Env, term)) {
        return typeof(double);
      }
      
      if(Imports.erldotnet_is_number(Env, term)) {
        if(Imports.erldotnet_is_int32(Env, term)) {
          return typeof(Int32);
        }
        if(Imports.erldotnet_is_int64(Env, term)) {
          return typeof(Int64);
        }
        return typeof(ErlNifTerm);
      }
      if(Imports.erldotnet_is_pid(Env, term)) {
        return typeof(Pid);
      }
      if(Imports.erldotnet_is_string(Env, term)) {
        return typeof(String);
      }
      if(Imports.erldotnet_is_binary(Env, term)) {
        return typeof(byte[]);
      }
      if(Imports.erldotnet_is_pointer_resource(Env, term)) {
        return typeof(ObjectReference);
      }
      return typeof(ErlNifTerm);
    }

    // To differentiate from 'object'
    private class ObjectReference{};
     
    public static object Coerce(ErlNifTerm term, Type type) {
      if(!Imports.erldotnet_is_valid_term(Env, term)) { return null; }

      if(type == typeof(Object)) {
        return ExtractAuto(term);
      }
      if(type == typeof(ObjectReference)) {
        return GetObjectReference(term);
      }
      if(type == typeof(ErlangCallback)) {
        return GetObjectReference(term);
      }
      if(type == typeof(String)) {
        return NativeToString(term);
      }
      if(type == typeof(Pid)) {
        return Imports.erldotnet_get_pid(Env, term);
      }
      if(type == typeof(Atom)) {
        return Coerce<String>(term) switch 
        {
          "undefined" => null,
          String Other => new Atom(Other)
        };
      }
      if(type == typeof(Int32)) {
        return NativeToInt32(term);
      }
      if(type == typeof(Int64)) {
        return NativeToInt64(term);
      }
      if(type == typeof(ErlNifTerm)) {
        return term;
      }

      if(type.IsRecord()) {
        var obj = Activator.CreateInstance(type);

        type.GetProperties()
          .ToList()
          .ForEach(x =>  {
                  var fieldName = MakeAtom(x.Name.ToLower());
                  var value= Imports.erldotnet_get_map_value(Env, fieldName, term);
                  var transformed = Coerce(value, x.PropertyType);
                  x.SetValue(obj, transformed);
              });

        return obj;
      }

      if(type.IsTuple()) {
        var argumentTypes = type.GetGenericArguments();
        var tupleLength = TupleLength(term);
        if(tupleLength < 0) {
          return null;
        }

        var convertedArgs = argumentTypes.Select((t, i) => Coerce(TupleElement(term, i), t));

        ConstructorInfo method = type.GetConstructor(argumentTypes);

        if(method == null) {
          return null;
        }

        return method.Invoke(convertedArgs.ToArray());
      }
      if(type == typeof(byte[])) {
        return NativeToBinary(term);
      }
      return null;
    }

    public static bool IsPid(ErlNifTerm term) {
      return Imports.erldotnet_is_pid(Env, term);
    }

    public static int TupleLength(ErlNifTerm term) {
      return Imports.erldotnet_tuple_length(Env, term);
    }

    public static ErlNifTerm TupleElement(ErlNifTerm term, int element) {
      return Imports.erldotnet_tuple_element(Env, element, term);
    }

    public static String NativeToString(ErlNifTerm term) {
      int length = 0;

      if((length = Imports.erldotnet_string_or_atom_length(Env, term)) > 0) {
        int allocLength = length + 1;
        IntPtr ptr = Marshal.AllocHGlobal(allocLength);
        Imports.erldotnet_term_to_string(Env, ptr, (uint)allocLength, term);
        String str = Marshal.PtrToStringAnsi(ptr);
        Marshal.FreeHGlobal(ptr);
        return str;
      }
      return String.Empty;
    }

    public static Byte[] NativeToBinary(ErlNifTerm term) {
      int length = 0;
      if((length = Imports.erldotnet_binary_length(Env, term)) > 0) {
        var ptr = Imports.erldotnet_binary_pointer(Env, term);
        var data = new byte[length];
        Marshal.Copy(ptr, data, 0, length);
        return data;
      }
      return new byte[] {};
    }

    public static Int32 NativeToInt32(ErlNifTerm term) {
      return Imports.erldotnet_term_to_int32(Env, term);
    }

    public static Int64 NativeToInt64(ErlNifTerm term) {
      return Imports.erldotnet_term_to_int64(Env, term);
    }
    
  }

  internal static class TypeExtensions
  {
    internal static bool IsTuple(this Type type) {
      var tupleTypes = new Type[]
      { typeof(Tuple<,>),
        typeof(Tuple<,,>),
        typeof(Tuple<,,,>),
        typeof(Tuple<,,,,>),
        typeof(Tuple<,,,,,>),
        typeof(Tuple<,,,,,,>),
        typeof(Tuple<,,,,,,,>),
      };

      return (type.IsGenericType && tupleTypes.Any(x => x == type.GetGenericTypeDefinition()));
    }

    public static bool IsRecord(this Type type) => type.GetMethod("<Clone>$") != null; // yikes
  }

}
