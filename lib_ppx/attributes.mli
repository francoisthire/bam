open Ppxlib

include module type of Attribute

module Core_type : sig
  val update : core_type -> Runtime.t -> core_type * Runtime.t
end

module Label_declaration : sig
  val update : label_declaration -> Runtime.t -> label_declaration * Runtime.t
end

module Constructor_declaration : sig
  val update :
    constructor_declaration -> Runtime.t -> constructor_declaration * Runtime.t
end

module Type_declaration : sig
  val update : type_declaration -> Runtime.t -> type_declaration * Runtime.t
end
