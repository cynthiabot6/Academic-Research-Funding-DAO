import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const researcher = accounts.get("wallet_1")!;
const voter1 = accounts.get("wallet_2")!;
const voter2 = accounts.get("wallet_3")!;

describe("Academic Research DAO", () => {
    it("allows submitting a research proposal", () => {
        const submitProposal = simnet.callPublicFn(
            "dao",
            "submit-proposal",
            [
                Cl.stringAscii("Quantum Computing Research"),
                Cl.uint(1000000)
            ],
            researcher
        );
        expect(submitProposal.result).toBeOk(Cl.uint(1));
    });
   
    
    
    

    it("prevents double voting", () => {
      const voteResult = simnet.callPublicFn(
          "dao",
          "vote",
          [Cl.uint(1)],
          voter1
      );
      expect(voteResult.result).toBeErr(Cl.error(Cl.uint(101)));
    });
});
