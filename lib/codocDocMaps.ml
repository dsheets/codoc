(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open DocOck.Paths

(* Polymorphic fold class hierarchies *)

module Identifier = struct
  include Identifier

  class type ['acc,'a,'b] module_fold = object
    method root : 'acc -> 'a -> string -> 'b
    method argument : 'acc -> 'a signature -> int -> string -> 'b
    method module_ : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] module_type_fold = object
    method module_type : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] datatype_fold = object
    method type_ : 'acc -> 'a signature -> string -> 'b
    method core_type : 'acc -> string -> 'b
  end

  class type ['acc,'a,'b] variant_constructor_fold = object
    method constructor : 'acc -> 'a datatype -> string -> 'b
  end

  class type ['acc,'a,'b] field_fold = object
    method field : 'acc -> 'a datatype -> string -> 'b
  end

  class type ['acc,'a,'b] extension_fold = object
    method extension : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] exception_fold = object
    method exception_ : 'acc -> 'a signature -> string -> 'b
    method core_exception : 'acc -> string -> 'b
  end

  class type ['acc,'a,'b] value_fold = object
    method value : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] class_fold = object
    method class_ : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] class_type_fold = object
    method class_type : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] method_fold = object
    method method_ : 'acc -> 'a class_signature -> string -> 'b
  end

  class type ['acc,'a,'b] instance_variable_fold = object
    method instance_variable : 'acc -> 'a class_signature -> string -> 'b
  end

  class type ['acc,'a,'b] label_fold = object
    method label : 'acc -> 'a parent -> string -> 'b
  end

  class type ['acc,'a,'b] signature_fold = object
    inherit ['acc,'a,'b] module_fold
    inherit ['acc,'a,'b] module_type_fold
  end

  class type ['acc,'a,'b] class_signature_fold = object
    inherit ['acc,'a,'b] class_fold
    inherit ['acc,'a,'b] class_type_fold
  end

  class type ['acc,'a,'b] container_fold = object
    inherit ['acc,'a,'b] signature_fold
    inherit ['acc,'a,'b] class_signature_fold
  end

  class type ['acc,'a,'b] parent_fold = object
    inherit ['acc,'a,'b] container_fold
    inherit ['acc,'a,'b] datatype_fold
  end

  class type ['acc,'a,'b] any_fold = object
    inherit ['acc,'a,'b] parent_fold
    inherit ['acc,'a,'b] variant_constructor_fold
    inherit ['acc,'a,'b] extension_fold
    inherit ['acc,'a,'b] exception_fold
    inherit ['acc,'a,'b] field_fold
    inherit ['acc,'a,'b] value_fold
    inherit ['acc,'a,'b] method_fold
    inherit ['acc,'a,'b] instance_variable_fold
    inherit ['acc,'a,'b] label_fold
  end

  class virtual ['acc,'a,'b] any_parent_fold = object (self : 'self)
    constraint 'self = ('acc,'a,'b) #any_fold

    method private virtual parent : 'acc -> 'a any -> 'a parent -> string -> 'b

    method module_ a p n =
      self#parent a (Module (p, n)) (parent_of_signature p) n
    method module_type a p n =
      self#parent a (ModuleType (p, n)) (parent_of_signature p) n
    method type_ a p n =
      self#parent a (Type (p, n)) (parent_of_signature p) n
    method constructor a p n =
      self#parent a (Constructor (p, n)) (parent_of_datatype p) n
    method field a p n =
      self#parent a (Field (p, n)) (parent_of_datatype p) n
    method extension a p n =
      self#parent a (Extension (p, n)) (parent_of_signature p) n
    method exception_ a p n =
      self#parent a (Exception (p, n)) (parent_of_signature p) n
    method value a p n =
      self#parent a (Value (p, n)) (parent_of_signature p) n
    method class_ a p n =
      self#parent a (Class (p, n)) (parent_of_signature p) n
    method class_type a p n =
      self#parent a (ClassType (p, n)) (parent_of_signature p) n
    method method_ a p n =
      self#parent a (Method (p, n)) (parent_of_class_signature p) n
    method instance_variable a p n =
      self#parent a (InstanceVariable (p, n)) (parent_of_class_signature p) n
    method label a p n =
      self#parent a (Label (p, n)) p n
  end

  class virtual ['acc,'a,'b] any_container_fragment_fold = object (self : 'self)
    constraint 'self = ('acc,'a,'b) #any_fold

    method private virtual signature :
      'acc -> 'a signature -> 'a parent -> string -> 'b

    method private virtual class_signature :
      'acc -> 'a class_signature -> 'a parent -> string -> 'b

    method private virtual fragment :
      'acc -> 'a any -> 'a parent -> string -> 'b

    method module_ a p n =
      self#signature a (Module (p, n)) (parent_of_signature p) n
    method module_type a p n =
      self#signature a (ModuleType (p, n)) (parent_of_signature p) n
    method type_ a p n =
      self#fragment a (Type (p, n)) (parent_of_signature p) n
    method constructor a p n =
      self#fragment a (Constructor (p, n)) (parent_of_datatype p) n
    method field a p n =
      self#fragment a (Field (p, n)) (parent_of_datatype p) n
    method extension a p n =
      self#fragment a (Extension (p, n)) (parent_of_signature p) n
    method exception_ a p n =
      self#fragment a (Exception (p, n)) (parent_of_signature p) n
    method value a p n =
      self#fragment a (Value (p, n)) (parent_of_signature p) n
    method class_ a p n =
      self#class_signature a (Class (p, n)) (parent_of_signature p) n
    method class_type a p n =
      self#class_signature a (ClassType (p, n)) (parent_of_signature p) n
    method method_ a p n =
      self#fragment a (Method (p, n)) (parent_of_class_signature p) n
    method instance_variable a p n =
      self#fragment a (InstanceVariable (p, n)) (parent_of_class_signature p) n
    method label a p n =
      self#fragment a (Label (p, n)) p n
  end

  let fold_any fold a = function
    | Root (root, name)          -> fold#root a root name
    | CoreType name              -> fold#core_type a name
    | CoreException name         -> fold#core_exception a name
    | Argument (p, i, name)      -> fold#argument a p i name
    | Module (p, name)           -> fold#module_ a p name
    | ModuleType (p, name)       -> fold#module_type a p name
    | Type (p, name)             -> fold#type_ a p name
    | Constructor (p, name)      -> fold#constructor a p name
    | Field (p, name)            -> fold#field a p name
    | Extension (p, name)        -> fold#extension a p name
    | Exception (p, name)        -> fold#exception_ a p name
    | Value (p, name)            -> fold#value a p name
    | Class (p, name)            -> fold#class_ a p name
    | ClassType (p, name)        -> fold#class_type a p name
    | Method (p, name)           -> fold#method_ a p name
    | InstanceVariable (p, name) -> fold#instance_variable a p name
    | Label (p, name)            -> fold#label a p name

  class virtual ['acc,'a,'b] any_root_fold = object (self : 'self)
    constraint 'self = ('acc,'a,'b) #any_fold
    inherit ['acc,'a,'b] any_parent_fold

    method core_type _ _ = None
    method core_exception _ _ = None
    method argument a p _ _ = fold_any self a (any p)
    method private parent a _ p _ = fold_any self a (any p)
  end

  let fold_signature fold a : 'a signature -> 'b = function
    | Root (root, name)     -> fold#root a root name
    | Argument (p, i, name) -> fold#argument a p i name
    | Module (p, name)      -> fold#module_ a p name
    | ModuleType (p, name)  -> fold#module_type a p name

  class virtual ['acc,'a,'b] signature_signature_fold = object (self : 'self)
    constraint 'self = ('acc,'a,'b) #signature_fold

    method private virtual signature :
        'acc -> 'a signature -> 'a signature -> string -> 'b

    method module_ a p n =
      self#signature a (Module (p, n)) p n
    method module_type a p n =
      self#signature a (ModuleType (p, n)) p n
  end

  class virtual ['acc,'a,'b] signature_root_fold = object (self : 'self)
    constraint 'self = ('acc,'a,'b) #signature_fold
    inherit ['acc,'a,'b] signature_signature_fold

    method argument a p _ _ = fold_signature self a p
    method private signature a _ p _ = fold_signature self a p
  end

  let fold_class_signature fold a : 'a class_signature -> 'b = function
    | Class (p, name)     -> fold#class_ a p name
    | ClassType (p, name) -> fold#class_type a p name

  class virtual ['acc,'a,'b] class_signature_signature_fold =
    object (self : 'self)
    constraint 'self = ('acc,'a,'b) #container_fold

    method private virtual class_signature :
        'acc -> 'a class_signature -> 'a signature -> string -> 'b

    method class_ a p n =
      self#class_signature a (Class (p, n)) p n
    method class_type a p n =
      self#class_signature a (ClassType (p, n)) p n
  end

  class virtual ['acc,'a,'b] class_signature_root_fold = object (self : 'self)
    constraint 'self = ('acc,'a,'b) #container_fold
    inherit ['acc,'a,'b] signature_root_fold
    inherit ['acc,'a,'b] class_signature_signature_fold

    method private class_signature a _ p _ = fold_signature self a p
  end

end

module Path_resolved = struct
  include Path.Resolved

  class type ['acc, 'a, 'b, 'c] module_fold = object
    constraint 'c = [< kind > `Module]
    method module_ : 'acc -> 'a module_ -> string -> 'b
    method apply : 'acc -> 'a module_ -> 'a Path.module_ -> 'b
    method subst : 'acc -> 'a module_type -> ('a,'c) t -> 'b
    method subst_alias : 'acc -> 'a module_ -> ('a,'c) t -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc, 'a, 'b, 'c] module_type_fold = object
    constraint 'c = [< kind > `ModuleType]
    method module_type : 'acc -> 'a module_ -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc, 'a, 'b, 'c] datatype_fold = object
    constraint 'c = [< kind > `Type]
    method type_ : 'acc -> 'a module_ -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc, 'a, 'b, 'c] class_fold = object
    constraint 'c = [< kind > `Class]
    method class_ : 'acc -> 'a module_ -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc, 'a, 'b, 'c] class_type_fold = object
    constraint 'c = [< kind > `ClassType]
    method class_type : 'acc -> 'a module_ -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc, 'a, 'b, 'c] class_signature_fold = object
    inherit ['acc, 'a, 'b, 'c] class_fold
    inherit ['acc, 'a, 'b, 'c] class_type_fold
  end

  class type ['acc, 'a, 'b, 'c] type_fold = object
    inherit ['acc, 'a, 'b, 'c] class_signature_fold
    inherit ['acc, 'a, 'b, 'c] datatype_fold
  end

  class type ['acc, 'a, 'b, 'c] any_fold = object
    inherit ['acc, 'a, 'b, 'c] module_fold
    inherit ['acc, 'a, 'b, 'c] module_type_fold
    inherit ['acc, 'a, 'b, 'c] type_fold
  end

  class virtual ['acc, 'a, 'b] any_parent_fold = object (self : 'self)
    constraint 'self = ('acc, 'a, 'b, kind) #any_fold

    method private virtual parent : 'acc -> 'a any -> 'a module_ -> string -> 'b

    method module_ a p n     = self#parent a (Module (p, n)) p n
    method module_type a p n = self#parent a (ModuleType (p, n)) p n
    method type_ a p n       = self#parent a (Type (p, n)) p n
    method class_ a p n      = self#parent a (Class (p, n)) p n
    method class_type a p n  = self#parent a (ClassType (p, n)) p n
  end

  let fold_any fold a = function
    | Identifier i         -> fold#identifier a i
    | Subst (rsub, li)     -> fold#subst a rsub li
    | SubstAlias (rsub, li)-> fold#subst_alias a rsub li
    | Module (p, name)     -> fold#module_ a p name
    | Apply (p, path)      -> fold#apply a p path
    | ModuleType (p, name) -> fold#module_type a p name
    | Type (p, name)       -> fold#type_ a p name
    | Class (p, name)      -> fold#class_ a p name
    | ClassType (p, name)  -> fold#class_type a p name
end

module Fragment_resolved = struct
  include Fragment.Resolved

  class type ['acc,'a] root_fold = object
    method root : 'acc -> 'a
  end

  class type ['acc,'a,'b,'c,'d] module_fold = object
    constraint 'c = [< kind > `Module]
    constraint 'd = [< sort > `Branch]
    method module_ : 'acc -> 'a signature -> string -> 'b
    method subst : 'acc -> 'a Path.Resolved.module_type -> ('a,'c,'d) raw -> 'b
    method subst_alias : 'acc -> 'a Path.Resolved.module_ -> ('a,'c,'d) raw -> 'b
  end

  class type ['acc,'a,'b,'c,'d] signature_fold = object
    inherit ['acc,'b] root_fold
    inherit ['acc,'a,'b,'c,'d] module_fold
  end

  class type ['acc,'a,'b] datatype_fold = object
    method type_ : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] class_fold = object
    method class_ : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] class_type_fold = object
    method class_type : 'acc -> 'a signature -> string -> 'b
  end

  class type ['acc,'a,'b] type_fold = object
    inherit ['acc,'a,'b] datatype_fold
    inherit ['acc,'a,'b] class_fold
    inherit ['acc,'a,'b] class_type_fold
  end

  class type ['acc,'a,'b,'c,'d] any_fold = object
    inherit ['acc,'a,'b,'c,'d] module_fold
    inherit ['acc,'a,'b] type_fold
  end

  class type ['acc,'a,'b,'c,'d] raw_fold = object
    inherit ['acc,'b] root_fold
    inherit ['acc,'a,'b,'c,'d] any_fold
  end

  class virtual ['acc,'a,'b,'kind,'sort] any_parent_fold = object (self : 'self)
    constraint 'self = ('acc,'a,'b,'kind,'sort) #any_fold

    method private virtual parent :
        'acc -> 'a any -> 'a signature -> string -> 'b

    method module_ a p n    = self#parent a (Module (p, n)) p n
    method type_ a p n      = self#parent a (Type (p, n)) p n
    method class_ a p n     = self#parent a (Class (p, n)) p n
    method class_type a p n = self#parent a (ClassType (p, n)) p n
  end

  let rec fold_any fold a : 'a any -> 'b = function
    | Subst (rsub, li)     -> fold#subst a rsub li
    | SubstAlias (rsub, li)-> fold#subst_alias a rsub li
    | Module (p, name)     -> fold#module_ a p name
    | Type (p, name)       -> fold#type_ a p name
    | Class (p, name)      -> fold#class_ a p name
    | ClassType (p, name)  -> fold#class_type a p name
end

module Reference_resolved = struct
  include Reference.Resolved

  class type ['acc,'a,'b,'c] module_fold = object
    constraint 'c = [< kind > `Module]
    method module_ : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] module_type_fold = object
    constraint 'c = [< kind > `ModuleType]
    method module_type : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] signature_fold = object
    inherit ['acc,'a,'b,'c] module_fold
    inherit ['acc,'a,'b,'c] module_type_fold
  end

  class type ['acc,'a,'b,'c] extension_fold = object
    constraint 'c = [< kind > `Extension]
    method extension : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] exception_fold = object
    constraint 'c = [< kind > `Exception]
    method exception_ : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] type_extension_fold = object
    inherit ['acc,'a,'b,'c] extension_fold
    inherit ['acc,'a,'b,'c] exception_fold
  end

  class type ['acc,'a,'b,'c] variant_constructor_fold = object
    constraint 'c = [< kind > `Constructor]
    method constructor : 'acc -> 'a datatype -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] constructor_fold = object
    inherit ['acc,'a,'b,'c] type_extension_fold
    inherit ['acc,'a,'b,'c] variant_constructor_fold
  end

  class type ['acc,'a,'b,'c] class_fold = object
    constraint 'c = [< kind > `Class]
    method class_ : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] class_type_fold = object
    constraint 'c = [< kind > `ClassType]
    method class_type : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] class_signature_fold = object
    inherit ['acc,'a,'b,'c] class_fold
    inherit ['acc,'a,'b,'c] class_type_fold
  end

  class type ['acc,'a,'b,'c] datatype_fold = object
    constraint 'c = [< kind > `Type]
    method type_ : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] type_fold = object
    inherit ['acc,'a,'b,'c] class_signature_fold
    inherit ['acc,'a,'b,'c] datatype_fold
  end

  class type ['acc,'a,'b,'c] container_fold = object
    inherit ['acc,'a,'b,'c] signature_fold
    inherit ['acc,'a,'b,'c] class_signature_fold
  end

  class type ['acc,'a,'b,'c] parent_fold = object
    inherit ['acc,'a,'b,'c] container_fold
    inherit ['acc,'a,'b,'c] type_fold
  end

  class type ['acc,'a,'b,'c] field_fold = object
    constraint 'c = [< kind > `Field]
    method field : 'acc -> 'a datatype -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] value_fold = object
    constraint 'c = [< kind > `Value]
    method value : 'acc -> 'a signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] method_fold = object
    constraint 'c = [< kind > `Method]
    method method_ : 'acc -> 'a class_signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] instance_variable_fold = object
    constraint 'c = [< kind > `InstanceVariable]
    method instance_variable : 'acc -> 'a class_signature -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] label_fold = object
    constraint 'c = [< kind > `Label]
    method label : 'acc -> 'a parent -> string -> 'b
    method identifier : 'acc -> ('a,'c) Identifier.t -> 'b
  end

  class type ['acc,'a,'b,'c] any_fold = object
    inherit ['acc,'a,'b,'c] parent_fold
    inherit ['acc,'a,'b,'c] constructor_fold
    inherit ['acc,'a,'b,'c] field_fold
    inherit ['acc,'a,'b,'c] value_fold
    inherit ['acc,'a,'b,'c] method_fold
    inherit ['acc,'a,'b,'c] instance_variable_fold
    inherit ['acc,'a,'b,'c] label_fold
  end

  class virtual ['acc,'a,'b] any_parent_fold = object (self : 'self)
    constraint 'self = ('acc,'a, 'b, kind) #any_fold

    method private virtual parent : 'acc -> 'a any -> 'a parent -> string -> 'b
    method private parent_signature a reference parent_signature name =
      self#parent a reference (parent_of_signature parent_signature) name
    method private parent_class a reference parent_class name =
      self#parent a reference (parent_of_class_signature parent_class) name
    method private parent_type a reference parent_type name =
      self#parent a reference (parent_of_datatype parent_type) name

    method module_ a p n     = self#parent_signature a (Module (p, n)) p n
    method module_type a p n = self#parent_signature a (ModuleType (p, n)) p n
    method type_ a p n       = self#parent_signature a (Type (p, n)) p n
    method constructor a p n = self#parent_type a (Constructor (p, n)) p n
    method field a p n       = self#parent_type a (Field (p, n)) p n
    method extension a p n   = self#parent_signature a (Extension (p, n)) p n
    method exception_ a p n  = self#parent_signature a (Exception (p, n)) p n
    method value a p n       = self#parent_signature a (Value (p, n)) p n
    method class_ a p n      = self#parent_signature a (Class (p, n)) p n
    method class_type a p n  = self#parent_signature a (ClassType (p, n)) p n
    method method_ a p n     = self#parent_class a (Method (p, n)) p n
    method instance_variable a p n =
      self#parent_class a (InstanceVariable (p, n)) p n
    method label a p n       = self#parent a (Label (p, n)) p n
  end

  let fold_any fold a = function
    | Identifier ident           -> fold#identifier a ident
    | Module (p, name)           -> fold#module_ a p name
    | ModuleType (p, name)       -> fold#module_type a p name
    | Type (p, name)             -> fold#type_ a p name
    | Constructor (p, name)      -> fold#constructor a p name
    | Field (p, name)            -> fold#field a p name
    | Extension (p, name)        -> fold#extension a p name
    | Exception (p, name)        -> fold#exception_ a p name
    | Value (p, name)            -> fold#value a p name
    | Class (p, name)            -> fold#class_ a p name
    | ClassType (p, name)        -> fold#class_type a p name
    | Method (p, name)           -> fold#method_ a p name
    | InstanceVariable (p, name) -> fold#instance_variable a p name
    | Label (p, name)            -> fold#label a p name
end

(* Constructed maps *)

module type ROOT = sig
  type t

  class virtual named : object ('self)
    constraint 'self = (unit, t, string) #Identifier.any_fold

    method root_name : unit -> t -> string
  end
end

module type ROOTED_MAPS = sig
  type root

  val root_of_ident : root Identifier.any -> (root * string) option
  val root_of_ident_signature : root Identifier.signature -> root * string
  val root_of_ident_class_signature :
    root Identifier.class_signature -> root * string

  class string_of_ident_map : object
    inherit [unit, root, string] Identifier.any_fold
    method root_name : unit -> root -> string
  end
  val string_of_ident_map : string_of_ident_map
  val string_of_ident : root Identifier.any -> string

  val name_of_root : root -> string

  val replace_ident_module_root
    : root -> root Identifier.module_ -> root Identifier.module_
end

module Make(Root : ROOT) : ROOTED_MAPS with type root = Root.t = struct
  type root = Root.t

  class root_of_ident_map :
    [unit, root, (root * string) option] Identifier.any_fold =
  object (self)
    inherit [unit, root, (root * string) option] Identifier.any_root_fold

    method root () r name = Some (r,name)
  end
  let root_of_ident_map = new root_of_ident_map
  let root_of_ident = Identifier.fold_any root_of_ident_map ()

  class root_of_ident_class_signature_map :
    [unit, root, root * string] Identifier.container_fold =
  object (self)
    inherit [unit, root, root * string] Identifier.class_signature_root_fold

    method root () r name = (r, name)
  end
  let root_of_ident_class_signature_map = new root_of_ident_class_signature_map
  let root_of_ident_class_signature =
    Identifier.fold_class_signature root_of_ident_class_signature_map ()

  class root_of_ident_signature_map :
    [unit, root, root * string] Identifier.signature_fold =
  object (self)
    inherit [unit, root, root * string] Identifier.signature_root_fold

    method root () r name = (r, name)
  end
  let root_of_ident_signature_map = new root_of_ident_signature_map
  let root_of_ident_signature =
    Identifier.fold_signature root_of_ident_signature_map ()

  class string_of_ident_map = object (self : 'self)
    constraint 'self = (unit, root, string) #Identifier.any_fold

    inherit [unit, root, string] Identifier.any_parent_fold
    inherit Root.named

    method root () r _name = self#root_name () r
    method core_type () name = name
    method core_exception () name = name
    method argument () parent i name =
      let parent = Identifier.any parent in
      (Identifier.fold_any self () parent)^"."^(string_of_int i)^name
    method private parent () ident parent name =
      (Identifier.fold_any self () (Identifier.any parent))^"."^name
  end

  let string_of_ident_map = new string_of_ident_map

  let string_of_ident = Identifier.fold_any string_of_ident_map ()

  let name_of_root = string_of_ident_map#root_name ()

  (* TODO: could be more parametric *)
  class replace_ident_signature_root_map new_root :
    [unit, root, root Identifier.signature] Identifier.signature_fold =
  object (self)
    method root () _r name = Identifier.Root (new_root, name)
    method argument () p n arg_name =
      Identifier.(Argument (fold_signature self () p, n, arg_name))
    method module_ () p mod_name =
      Identifier.(Module (fold_signature self () p, mod_name))
    method module_type () p mod_type_name =
      Identifier.(ModuleType (fold_signature self () p, mod_type_name))
  end

  let replace_ident_signature_root_map new_root =
    new replace_ident_signature_root_map new_root
  let replace_ident_signature_root new_root =
    Identifier.fold_signature (replace_ident_signature_root_map new_root) ()
  let replace_ident_module_root new_root module_ =
    let signature = Identifier.signature_of_module module_ in
    Identifier.(match replace_ident_signature_root new_root signature with
    | Root (r,n) -> Root (r,n)
    | Argument (p,k,n) -> Argument (p,k,n)
    | Module (p,n) -> Module (p,n)
    | ModuleType _ -> assert false (* TODO: investigate *)
    )

end

(* Utilities *)

let parent_list ident =
  let rec path acc ident = Identifier.(match ident with
    | Root _ -> ident::acc
    | Module (p, _)
    | Argument (p, _, _)
    | ModuleType (p, _) -> path (ident::acc) p
  ) in
  List.(tl (rev (path [] ident)))
