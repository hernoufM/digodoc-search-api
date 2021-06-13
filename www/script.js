var last_id = 0;
var pattern = "~";

var main_div = document.getElementById("main_content");
var load_div = document.createElement("div");
load_div.setAttribute("id", "load_div");


const sendRequest = async () => {
    console.log("Send request id " + last_id + " pattern " + pattern);
    const response = await fetch('http://localhost:49001/modules/'+ last_id + '/' + pattern);
    const myJson = await response.json(); //extract JSON from the http response
    if (myJson.length == 0){
        return false;
    }
    else{
        for(i in myJson){
            var module_div = document.createElement("div");
            module_div.innerHTML = JSON.stringify(myJson[i].mdl_id + ', ' + myJson[i].mdl_name);
            main_div.appendChild(module_div);
        }
        return true;
    }
}



window.onload = () => {

    let name = document.getElementById("search")

    name.onkeyup = () => {
        let re = name.value.toLowerCase();
        let divs = document.getElementsByClassName("packages-set");
        let cpt = 0;
        Array.prototype.forEach.call(divs, div => {
            let children = div.children;
            var h3 = children[0];
            var ol = children[1];
            var displayed = false ;
            Array.prototype.forEach.call( ol.children, li => {
                if (li.id.toLowerCase().includes(re)) {
                    li.style.display = '';
                    cpt++;
                    displayed = true;
                } else {
                    li.style.display = "none";
                }});
            if( displayed ){
                h3.style.display = ""
            } else {
                h3.style.display = "none"
            }
        });
        const input = re.trim();
            if(input.length > 0){
                pattern = input;
            }
            else {
                pattern = "~";
            }
            last_id = 0;
            console.log("KEYUP id=" +last_id + " input=" +input + " re="+re);
            main_div.innerHTML = '';
            sendRequest().then(function(added){
                if(added){
                    last_id = last_id + 50;
                    main_div.appendChild(load_div);
                }
            });
        /*let items_nbr = document.getElementById("item-number");
        let content = items_nbr.innerHTML.split(' ');
        content[0] = cpt;
        items_nbr.innerHTML = content.join(' ');*/
        
    }
}

var observer = new IntersectionObserver(function(entries) {

    if(entries[0].isIntersecting === true){

        console.log('Element is fully visible in screen');
        main_div.removeChild(load_div);
        /*const input = String.prototype.trim(text);
        if(input.length > 0){
            pattern = input;
        }
        else {
            pattern = "~";
        }
        last_id = 0;
        main_div.innerHTML = '';
    await sendRequest();*/
    sendRequest().then(function(added){
            if(added){
                last_id = last_id + 50;
                main_div.appendChild(load_div);
            }
        })
    }
}, { threshold: [1] });

sendRequest().then(function(_){ 
    last_id = last_id + 50;
    main_div.appendChild(load_div);
    observer.observe(document.querySelector("#load_div"));
});