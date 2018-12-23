const express = require("express");
const router = express.Router();
const fileuploderAPIs = require("./fileuploder");
router.route("/ReadFile").get((req, res) => {
    let reqstarttime = (new Date()).getTime();
    let params = req.query;
    fileuploderAPIs.ReadFile(params);
        
});

module.exports = router;