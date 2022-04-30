(** IO modules *)

module IO = Io.Make

module Net = Net.Make
(** The resulting {modtype:Cohttp_lwt.S.Net} module can be used to build the
    low-level client interfaces with {module:Cohttp_lwt.Connection.Make} and
    from that {module:Cohttp_lwt.Connection_cache.Make}. *)

(** client modules *)

(** simple, high-level interace *)

module Client = Client

(** server modules *)

module Static = Static
(** Serve static HTTP sites from a Mirage key-value store. *)

module Server = Server
