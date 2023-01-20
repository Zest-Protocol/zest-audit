// deno-lint-ignore-file
import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.3/index.ts';
import { assertEquals, assert } from 'https://deno.land/std@0.90.0/testing/asserts.ts';
import { Pool } from '../../interfaces/pool-v1-0.ts';
import { CoverPool } from '../../interfaces/cover-pool-v1-0.ts';
import { Loan } from '../../interfaces/loan-v1-0.ts';
import { LPToken } from '../../interfaces/lp-token.ts';
import { CPToken } from '../../interfaces/cp-token.ts';
import { Buffer } from "https://deno.land/std@0.110.0/node/buffer.ts";
import { TestUtils } from '../../interfaces/test-utils.ts';
import { Bridge } from '../../interfaces/bridge_real.ts';
import { Safe } from '../../interfaces/safe.ts';
import { Globals } from '../../interfaces/globals.ts';
import { SupplierInterface } from '../../interfaces/supplier_interface.ts';
import { SwapRouter } from '../../interfaces/swapRouter.ts';
import { 
  getHash,
  getReverseTxId,
  getTxId,
  getExpiration,
  swapperBuff,
  generateP2PKHTx,
  generateP2SHTx,
} from "../util.ts";

import {
  setContractOwner,
  initContractOwners,
  bootstrapApprovedContracts,
  addApprovedContract,
  runBootstrap,
  addBorrower,
  sendFundsP2SHTxs,
  registerSupplierTxs,
  finalizeOutboundTxs,
  consumeUint,
  getBP,
  finalizeDrawdown
} from '../../interfaces/common.ts';

import {
  LP_TOKEN,
  ZP_TOKEN,
  PAYMENT,
  REWARDS_CALC,
  LIQUIDITY_VAULT,
  CP_TOKEN,
  COLL_VAULT,
  FUNDING_VAULT,
  P2PKH_VERSION,
  HASH,
  XBTC,
  recipient,
  sender,
  preimage,
  ONE_DAY,
  CP_REWARDS_TOKEN,
  ERRORS,
  SWAP_ROUTER,
  XUSD_CONTRACT_SIMNET,
  USDA_CONTRACT_SIMNET,
  COVER_VAULT,
  SAFE
} from "../config.ts";

const MAX_MATURITY_LENGTH = 144 * 365 * 3; // 3 years

Clarinet.test({
  name: "Liquidity providers can send funds to the liquidity vault",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    let deployerWallet = accounts.get("deployer") as Account;
    let wallet_1 = accounts.get("wallet_1") as Account; // LP_1
    let wallet_2 = accounts.get("wallet_2") as Account; // LP_2
    let wallet_7 = accounts.get("wallet_7") as Account; // DELEGATE_1
    let assetMaps = chain.getAssetsMaps();
    let pool = new Pool(chain, deployerWallet);
    let loan = new Loan(chain, deployerWallet);

    pool.createPool(wallet_7.address,LP_TOKEN,ZP_TOKEN,PAYMENT,REWARDS_CALC,1000,1000,10_000_000_000,10_000_000_000,1,MAX_MATURITY_LENGTH,LIQUIDITY_VAULT,CP_TOKEN,COVER_VAULT,CP_REWARDS_TOKEN,XBTC,true);
    pool.finalizePool(wallet_7.address, LP_TOKEN, ZP_TOKEN, CP_TOKEN, 0);

    let supplierId = consumeUint(chain.mineBlock([...registerSupplierTxs(deployerWallet.address, deployerWallet.address, recipient, 10, 10, 500, 500, "supplier-1", 10_000_000_000)]).receipts[1].result.expectOk() as string);
    let fee = Number(Bridge.getSupplier(chain, supplierId, deployerWallet.address).expectSome().expectTuple()["inbound-fee"].expectSome());
    
    let block = chain.mineBlock([
      Safe.submit(
        `${deployerWallet.address}.safe`,
        `${deployerWallet.address}.initialize-swapper`,
        `${deployerWallet.address}.safe`,
        `${deployerWallet.address}.ft-none`,
        `${deployerWallet.address}.nft-none`,
        null,
        null,
        null,
        wallet_1.address
      )
    ]);
    let initializeId = (consumeUint(block.receipts[0].result.expectOk()));
    block = chain.mineBlock([
      Safe.confirm(
        `${deployerWallet.address}.safe`,
        initializeId,
        `${deployerWallet.address}.initialize-swapper`,
        `${deployerWallet.address}.safe`,
        `${deployerWallet.address}.ft-none`,
        `${deployerWallet.address}.nft-none`,
        wallet_2.address),
    ]);
    assertEquals(Bridge.getSwapper(chain, 0, deployerWallet.address).expectSome(), `${deployerWallet.address}.safe`);
  },
});


Clarinet.test({
  name: "Multisafe Pool Delegate can fund a loan",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    let deployerWallet = accounts.get("deployer") as Account;
    let wallet_1 = accounts.get("wallet_1") as Account; // LP_1
    let wallet_2 = accounts.get("wallet_2") as Account; // LP_2
    let wallet_7 = accounts.get("wallet_7") as Account; // DELEGATE_1
    let wallet_8 = accounts.get("wallet_8") as Account; // DELEGATE_1
    let assetMaps = chain.getAssetsMaps();
    let pool = new Pool(chain, deployerWallet);
    let loan = new Loan(chain, deployerWallet);

    pool.createPool(wallet_7.address,LP_TOKEN,ZP_TOKEN,PAYMENT,REWARDS_CALC,1000,1000,10_000_000_000,10_000_000_000,1,MAX_MATURITY_LENGTH,LIQUIDITY_VAULT,CP_TOKEN,COVER_VAULT,CP_REWARDS_TOKEN,XBTC,true);
    pool.finalizePool(wallet_7.address, LP_TOKEN, ZP_TOKEN, CP_TOKEN, 0);

    let supplierId = consumeUint(chain.mineBlock([...registerSupplierTxs(deployerWallet.address, deployerWallet.address, recipient, 10, 10, 500, 500, "supplier-1", 10_000_000_000)]).receipts[1].result.expectOk() as string);
    let fee = Number(Bridge.getSupplier(chain, supplierId, deployerWallet.address).expectSome().expectTuple()["inbound-fee"].expectSome());
    
    let block = chain.mineBlock([
      Safe.submit(
        `${deployerWallet.address}.safe`,
        `${deployerWallet.address}.initialize-swapper`,
        `${deployerWallet.address}.safe`,
        `${deployerWallet.address}.ft-none`,
        `${deployerWallet.address}.nft-none`,
        null,
        null,
        null,
        wallet_1.address
      )
    ]);
    let initializeId = (consumeUint(block.receipts[0].result.expectOk()));
    block = chain.mineBlock([
      Safe.confirm(
        `${deployerWallet.address}.safe`,initializeId, `${deployerWallet.address}.initialize-swapper`,`${deployerWallet.address}.safe`,`${deployerWallet.address}.ft-none`,`${deployerWallet.address}.nft-none`,wallet_2.address),
    ]);
    assertEquals(Bridge.getSwapper(chain, 0, deployerWallet.address).expectSome(), `${deployerWallet.address}.safe`);
  },
});


