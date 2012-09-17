$(function() {

  var TWITCOIN_TX_FEE = 0.002;

  var urlParams = function() {
    var params = {};

    location.search.replace(
      /([^?=&]+)(=([^&]*))?/g,
      function (match, key, full, value) {
        params[key] = decodeURIComponent(value);
      }
    );
    return params;
  };

  var initSessionMenu = function() {
    var $session_menu = $("#session-menu"),
      $dropdown = $("#session-menu .dropdown");

    $session_menu.find(".user").click(function() {
      if ($dropdown.is(":hidden")) {
        $dropdown.show();
        $session_menu.css({"box-shadow": "0 1px 5px rgba(0, 0, 0, 0.5)"});
      } else {
        $dropdown.hide();
        $session_menu.css({"box-shadow": "none"});
      }
      return false;
    });

    $dropdown.find("a").click(function() {
      $.ajax({
        url: this.href,
        type: 'DELETE',
        success: function() {
          window.location = "/";
        }
      });
      return false;
    });

    $session_menu.fadeIn();
  };

  var account = urlParams().account,
    $session_menu = $("#session-menu"),
    $send_form = $("#send-form"),
    $submit_button = $send_form.find("input[type='submit']"),
    $sender_account = $send_form.find("input[name='sender_account']");

  if (account) {
    $.getJSON("/api/account/" + account, function(data) {
      if (data.result) {
        var $user = $session_menu.find(".user"),
          $user_img = $user.find("img"),
          $user_name = $user.find(".name"),
          screen_name = data.result.screen_name,
          user_url = "https://twitter.com/" + screen_name,
          $account_data = $("#account-data"),
          $qrcode_popup = $("#qrcode-popup");

        $user.attr("href", user_url)
        $user.attr("title", "@" + screen_name);
        $user_img.attr("src", data.result.profile_img_url);
        $user_name.text(data.result.name);
        initSessionMenu();

        $sender_account.val(screen_name);

        if ($account_data.length) {
          var $balance = $account_data.find(".balance"),
            $address = $account_data.find(".address"),
            address_html = '<a href="bitcoin:' + data.result.address + '">'
              + data.result.address + '</a>&nbsp;&nbsp;'
              + '<a href="' + data.result.address_qrcode_path
              + '" class="qrcode-link">[QR Code]</a>';

          $balance.data("balance", data.result.balance);

          $balance.hide().text(data.result.balance + " Ƀ").fadeIn();
          $address.hide().html(address_html).fadeIn();

          $address.find(".qrcode-link").click(function() {
            $qrcode_popup.html('<img src="' + this.href + '">');
            $qrcode_popup.fadeIn();
            $(document.body).click(function() {
              $qrcode_popup.fadeOut();
            });
            return false;
          });
        }
      }
    });

    $submit_button.click(function() {
      var send_form = $send_form.get(0),
        $message = $send_form.find(".message"),
        sender_account = $sender_account.val(),
        receiver = $send_form.find("input[name='receiver']").val(),
        amount = parseFloat($send_form.find("input[name='amount']").val()),
        tx_data = '{' +
          '"sender_account":"' + sender_account + '",' +
          '"receiver":"' + receiver + '",' +
          '"amount":' + amount +
        '}';

      $message.hide();

      if (send_form.checkValidity && !send_form.checkValidity()) {
        return true; // don't bother if data is invalid
      }

      $submit_button.attr("disabled", true);

      $.ajax({
        type: "POST",
        url: $send_form.attr("action"),
        contentType: "application/json",
        data: tx_data,
        error: function(jqXHR, _status, error) {
          $message.removeClass("success").addClass("error");
          $message.hide().text("Error: " + error).fadeIn();
          $submit_button.attr("disabled", false);
        },
        success: function(data) {
          if (!data.result) {
            var generic_error_msg = "There was an error. Try again after "
              + "reloading the page.";

            $message.removeClass("success").addClass("error");
            $message.text(data.error || generic_error_msg).fadeIn();
          } else {
            var $balance = $("#account-data .balance"),
              new_balance = $balance.data("balance") - amount - TWITCOIN_TX_FEE,
              proper_rounded_new_balance = Math.round(1e8 * new_balance) / 1e8;

            $balance.data("balance", proper_rounded_new_balance);
            $balance.text(proper_rounded_new_balance + " Ƀ");
            $message.removeClass("error").addClass("success");
            $message.text("Transaction successful.").fadeIn();
          }
          $submit_button.attr("disabled", false);
        }
      });

      return false;
    });

    $send_form.submit(function() {
      // This works around buggy browsers that can actually do HTML5 form
      // validations but still submit a form even though they know that this
      // particular form is not valid.
      return false;
    });

    $(".site-link").each(function(_i, link) {
      link.href = link.href + "?account=" + account + "&ts=" + urlParams().ts;
    });
  }
});
