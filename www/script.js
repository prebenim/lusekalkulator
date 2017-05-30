$(document).ready(function(){
  $(document).on('shiny:connected', function(event) {

    var total = 1;
    $("#merd2").hide();
    $("#merd3").hide();
    $("#merd4").hide();
    $("#merd5").hide();
    $("#merd6").hide();
    $("#merd7").hide();
    $("#merd8").hide();
    $("#merd9").hide();
    $("#merd10").hide();
    $("#merd11").hide();
    $("#merd12").hide();
    $("#merd13").hide();
    $("#merd14").hide();
    $("#merd15").hide();
    $("#merd16").hide();
    $("#addmerd").click(function() {
    	$("#merd"+total + "x").hide();
      total++;
      $("#merd"+total).show();
    });


    $('[data-toggle="tooltip"]').tooltip();

		$("#merd1x").click(function() { $("#merd1").hide(); total--; });
		$("#merd2x").click(function() { $("#merd2").hide(); $("#merd1x").show(); total--; });
		$("#merd3x").click(function() { $("#merd3").hide(); $("#merd2x").show(); total--; });
		$("#merd4x").click(function() { $("#merd4").hide(); $("#merd3x").show(); total--; });
		$("#merd5x").click(function() { $("#merd5").hide(); $("#merd4x").show(); total--; });
		$("#merd6x").click(function() { $("#merd6").hide(); $("#merd5x").show(); total--; });
		$("#merd7x").click(function() { $("#merd7").hide(); $("#merd6x").show(); total--; });
		$("#merd8x").click(function() { $("#merd8").hide(); $("#merd7x").show(); total--; });
		$("#merd9x").click(function() { $("#merd9").hide(); $("#merd8x").show(); total--; });
		$("#merd10x").click(function() { $("#merd10").hide(); $("#merd9x").show(); total--; });
		$("#merd11x").click(function() { $("#merd11").hide(); $("#merd10x").show(); total--; });
		$("#merd12x").click(function() { $("#merd12").hide(); $("#merd11x").show(); total--; });
		$("#merd13x").click(function() { $("#merd13").hide(); $("#merd12x").show(); total--; });
		$("#merd14x").click(function() { $("#merd14").hide(); $("#merd13x").show(); total--; });
		$("#merd15x").click(function() { $("#merd15").hide(); $("#merd14x").show(); total--; });
		$("#merd16x").click(function() { $("#merd16").hide(); $("#merd15x").show(); total--; });

    $("#startblock").hide();
    $("#loknrblock").hide();    
    $('input[type=radio][name=smradio]').change(function() {
        if (this.value == 'auto') {
            $("#startblock").show();
            $("#smblock").hide();
            $("#loknrblock").show();
        }
        else if (this.value == 'manual') {
            $("#startblock").hide();
            $("#smblock").show();
            $("#loknrblock").hide();
        }
    });

    $("#manualdata").hide();
    $('input[type=radio][name=merddataradio]').change(function() {
        if (this.value == 'csv') {
            $("#manualdata").hide();
            $("#fileuploadblock").show();
        }
        else if (this.value == 'manual') {
            $("#manualdata").show();
            $("#fileuploadblock").hide();
        }
    });


    $("#file1").click(function() {
      $("#mainblock").show();
    });
    $("#calculate").click(function() {
      $("#mainblock").show();
    });
    setTimeout(function(){ $("#mainblock").hide(); }, 300);

  });
});