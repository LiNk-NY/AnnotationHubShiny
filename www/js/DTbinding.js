$(function() {
    $("#select_all_rows").click(function(){
        $(".selectable div table tbody tr").addClass("rowsSelected");
        $(".selectable div table").trigger("change");
    });

    $("#deselect_all_rows").click(function(){
        $(".selectable div table tbody tr").removeClass("rowsSelected");
        $(".selectable div table").trigger("change");
    });
});

$(document).on('click', '.selectable div table tbody tr', function(e){
   var el = $(this);
   if (!e.shiftKey){
       $(this).siblings().removeClass("rowsSelected");
   }
   $(this).addClass("rowsSelected", this.clicked);
   el.trigger("change");
});    


var isArray = function(someVar)
{
    return(Object.prototype.toString.call( someVar ) === '[object Array]');
}

var selectRowBinding = new Shiny.InputBinding();

$.extend(selectRowBinding, {
   find: function(scope) {
       return $(scope).find(".selectable");
   },
   getValue: function(el){
    tbl = $(el).find("table");
    var out = [];
    $rows = $(tbl).children().children('.rowsSelected');
    if($rows.length == 0) return -1;
    var oTable = $("#DataTables_Table_0").dataTable();

    $rows.each(function(row,v) {
        var aPos = oTable.fnGetPosition( this );
        var data = oTable.fnGetData(this);
        out[row] = [];
        for (var i = 0; i < data.length; i++) {
            var di = data[i];
            if (isArray(di)) di = di.join(",");
            out[row][i] = di;
            console.log("i is " + i + " and di is " + di);
        }
    });
    return out;
   },
   setValue: function(el, value) {
   },
   subscribe: function(el, callback) {
       $(el).on("change.selectRowBinding", function(e) {
           callback();
       });
   },
   unsubscribe: function(el) {
     $(el).off(".selectRowBinding");
   }
});
Shiny.inputBindings.register(selectRowBinding);
