
class FIleUploader {

    ReadFile(params) {
        var myfile = params.path;
            console.log($("#csvfile").length);
        /*var json = Papa.parse(myfile,
            {
                header: true,
                skipEmptyLines: true,
                complete: function (results) {
                    console.log("Dataframe:", JSON.stringify(results.data));
                    console.log("Column names:", results.meta.fields);
                    //console.log("Errors:", results.errors);
                    console.log("Without", results.data);
                }});*/
    }
}
module.exports = new FIleUploader();
