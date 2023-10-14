// this is an ugly hack to make dyld stuff compile

#define CCSHA384_OUTPUT_SIZE 42
#define CCSHA256_OUTPUT_SIZE 42

typedef void *ccdigest_ctx_t;

#define ccsha1_di()   nullptr;
#define ccsha256_di() nullptr;
#define ccsha384_di() nullptr;
#define ccdigest_init(a, b)
#define ccdigest_update(a, b, c, d)
#define ccdigest_final(a, b, c)
#define ccdigest_di_clear(a, b)
#define ccdigest_di_decl(di, bufname) uint8_t bufname[42];