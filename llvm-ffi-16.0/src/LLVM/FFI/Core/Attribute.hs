module LLVM.FFI.Core.Attribute (
    Name(Name),
    zeroext,
    signext,
    inreg,
    byval,
    sret,
    align,
    noalias,
    nocapture,
    nest,
    returned,
    nonnull,
    dereferenceable,
    dereferenceableOrNull,
    swiftself,
    swifterror,
    immarg,
    alignstack,
    allocsize,
    alwaysinline,
    builtin,
    cold,
    convergent,
    inaccessiblememonly,
    inaccessiblememOrArgmemonly,
    inlinehint,
    jumptable,
    minsize,
    naked,
    noJumpTables,
    nobuiltin,
    noduplicate,
    nofree,
    noimplicitfloat,
    noinline,
    nonlazybind,
    noredzone,
    indirectTlsSegRefs,
    noreturn,
    norecurse,
    willreturn,
    nosync,
    nounwind,
    nullPointerIsValid,
    optforfuzzing,
    optnone,
    optsize,
    patchableFunction,
    probeStack,
    readnone,
    readonly,
    stackProbeSize,
    noStackArgProbe,
    writeonly,
    argmemonly,
    returnsTwice,
    safestack,
    sanitizeAddress,
    sanitizeMemory,
    sanitizeThread,
    sanitizeHwaddress,
    sanitizeMemtag,
    speculativeLoadHardening,
    speculatable,
    ssp,
    sspreq,
    sspstrong,
    strictfp,
    uwtable,
    nocfCheck,
    shadowcallstack,
    ) where


-- data Function
-- data Parameter
newtype Name = Name String

-- fgrep '<dt><code class="docutils literal' /usr/share/doc/llvm-9*/html/LangRef.html

-- * Parameter attributes

zeroext :: Name
zeroext = Name "zeroext"

signext :: Name
signext = Name "signext"

inreg :: Name
inreg = Name "inreg"

byval :: Name
byval = Name "byval"

sret :: Name
sret = Name "sret"

align :: Name
align = Name "align"

noalias :: Name
noalias = Name "noalias"

nocapture :: Name
nocapture = Name "nocapture"

nest :: Name
nest = Name "nest"

returned :: Name
returned = Name "returned"

nonnull :: Name
nonnull = Name "nonnull"

dereferenceable :: Name
dereferenceable = Name "dereferenceable"

dereferenceableOrNull :: Name
dereferenceableOrNull = Name "dereferenceable_or_null"

swiftself :: Name
swiftself = Name "swiftself"

swifterror :: Name
swifterror = Name "swifterror"

immarg :: Name
immarg = Name "immarg"


-- * Function attributes

alignstack :: Name
alignstack = Name "alignstack"

allocsize :: Name
allocsize = Name "allocsize"

alwaysinline :: Name
alwaysinline = Name "alwaysinline"

builtin :: Name
builtin = Name "builtin"

cold :: Name
cold = Name "cold"

convergent :: Name
convergent = Name "convergent"

inaccessiblememonly :: Name
inaccessiblememonly = Name "inaccessiblememonly"

inaccessiblememOrArgmemonly :: Name
inaccessiblememOrArgmemonly = Name "inaccessiblemem_or_argmemonly"

inlinehint :: Name
inlinehint = Name "inlinehint"

jumptable :: Name
jumptable = Name "jumptable"

minsize :: Name
minsize = Name "minsize"

naked :: Name
naked = Name "naked"

noJumpTables :: Name
noJumpTables = Name "no-jump-tables"

nobuiltin :: Name
nobuiltin = Name "nobuiltin"

noduplicate :: Name
noduplicate = Name "noduplicate"

nofree :: Name
nofree = Name "nofree"

noimplicitfloat :: Name
noimplicitfloat = Name "noimplicitfloat"

noinline :: Name
noinline = Name "noinline"

nonlazybind :: Name
nonlazybind = Name "nonlazybind"

noredzone :: Name
noredzone = Name "noredzone"

indirectTlsSegRefs :: Name
indirectTlsSegRefs = Name "indirect-tls-seg-refs"

noreturn :: Name
noreturn = Name "noreturn"

norecurse :: Name
norecurse = Name "norecurse"

willreturn :: Name
willreturn = Name "willreturn"

nosync :: Name
nosync = Name "nosync"

nounwind :: Name
nounwind = Name "nounwind"

nullPointerIsValid :: Name
nullPointerIsValid = Name "null-pointer-is-valid"

optforfuzzing :: Name
optforfuzzing = Name "optforfuzzing"

optnone :: Name
optnone = Name "optnone"

optsize :: Name
optsize = Name "optsize"

patchableFunction :: Name
patchableFunction = Name "patchable-function"

probeStack :: Name
probeStack = Name "probe-stack"

readnone :: Name
readnone = Name "readnone"

readonly :: Name
readonly = Name "readonly"

stackProbeSize :: Name
stackProbeSize = Name "stack-probe-size"

noStackArgProbe :: Name
noStackArgProbe = Name "no-stack-arg-probe"

writeonly :: Name
writeonly = Name "writeonly"

argmemonly :: Name
argmemonly = Name "argmemonly"

returnsTwice :: Name
returnsTwice = Name "returns_twice"

safestack :: Name
safestack = Name "safestack"

sanitizeAddress :: Name
sanitizeAddress = Name "sanitize_address"

sanitizeMemory :: Name
sanitizeMemory = Name "sanitize_memory"

sanitizeThread :: Name
sanitizeThread = Name "sanitize_thread"

sanitizeHwaddress :: Name
sanitizeHwaddress = Name "sanitize_hwaddress"

sanitizeMemtag :: Name
sanitizeMemtag = Name "sanitize_memtag"

speculativeLoadHardening :: Name
speculativeLoadHardening = Name "speculative_load_hardening"

speculatable :: Name
speculatable = Name "speculatable"

ssp :: Name
ssp = Name "ssp"

sspreq :: Name
sspreq = Name "sspreq"

sspstrong :: Name
sspstrong = Name "sspstrong"

strictfp :: Name
strictfp = Name "strictfp"

uwtable :: Name
uwtable = Name "uwtable"

nocfCheck :: Name
nocfCheck = Name "nocf_check"

shadowcallstack :: Name
shadowcallstack = Name "shadowcallstack"
