import { initializeLucid, lockFundsWithDatum, redeemFundsWithRedeemer } from './lucidService.js';

// Controller to lock funds with datum
const _lockFundsWithDatum = async (req, res) => {
    try {
        const { reviewId, reviewReferenceId, overallRating, timestamp } = req.body;

        console.log(reviewId, reviewReferenceId, overallRating, timestamp);
        
        const txHash = await lockFundsWithDatum(reviewId, reviewReferenceId, overallRating, timestamp);
        res.json({ txHash });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};
export { _lockFundsWithDatum as lockFundsWithDatum };

// Controller to redeem funds with redeemer
const _redeemFundsWithRedeemer = async (req, res) => {
    try {
        const { action } = req.body;
        const txHash = await redeemFundsWithRedeemer(action);
        res.json({ txHash });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
};
export { _redeemFundsWithRedeemer as redeemFundsWithRedeemer };
