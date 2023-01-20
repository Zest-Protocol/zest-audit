import { Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.3/index.ts';
import { Buffer } from "https://deno.land/std@0.110.0/node/buffer.ts";

class Safe {
  static submit(
    safeContractAddress: string,
    executor: string,
    safe: string,
    paramFt: string,
    paramNft: string,
    paramP: string,
    paramU: number,
    paramB: string,
    caller: string
  ) {
    return Tx.contractCall(
      safeContractAddress,
      'submit',
      [
        types.principal(executor),
        types.principal(safe),
        types.principal(paramFt),
        types.principal(paramNft),
        paramP ? types.some(types.principal(paramP)) : types.none(),
        paramU ? types.some(types.uint(paramU)) : types.none(),
        paramB ? types.buff(Buffer.from(paramB, "hex")) : types.none(),
      ],
      caller
    );
  }

  static confirm(
    safeContractAddress: string,
    txid: number,
    executor: string,
    safe: string,
    paramFt: string,
    paramNft: string,
    caller: string
  ) {
    return Tx.contractCall(
      safeContractAddress,
      'confirm',
      [
        types.uint(txid),
        types.principal(executor),
        types.principal(safe),
        types.principal(paramFt),
        types.principal(paramNft),
      ],
      caller
    );
  }
}

export { Safe };