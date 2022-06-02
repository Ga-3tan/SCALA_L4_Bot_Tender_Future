document.addEventListener('DOMContentLoaded', function(){
    const socket = new WebSocket("ws://" + location.host + "/subscribe");
    socket.onmessage = function(ev){ boardMessage.innerHTML = ev.data }
}, false);

function submitMessageForm(){
    fetch(
        "/send", 
        {
            method: "POST",
            body: JSON.stringify({msg: messageInput.value})
        }
    ).then(response => response.json())
    .then(json => {
        if(!json.err) {
            messageInput.value = ""
        }
        errorDiv.innerText = json.err
    })
}
