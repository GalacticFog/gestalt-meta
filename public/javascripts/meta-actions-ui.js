          /* Initialize 'select' element-type (dropdown is a jQuery-plugin) */
          $(document).ready(function() {
            $('select').material_select();
            $('modal').material_select();
            $('.collapsible').collapsible();
          });

          const GF_CONTROLS_FORM = 'gestalt-action-form';
          const GF_CONTROLS_EXECUTE = 'gestalt-execute';
          const GF_CONTROLS_CANCEL = 'gestalt-cancel';
          const GF_CONTROLS_ERROR_MODAL = 'gestalt-error'

          const get = (id) => document.getElementById(id);
          const setText = (id, text) => get(id).textContent = text;
          const setHtml = (id, html) => get(id).innerHTML = html;
          const invalidNumber = (event) => event.target.setCustomValidity('Invalid port number.');
          const resetValidation = (event) => event.target.setCustomValidity('');

          const withInstructions = (instructions) => {
            get('instruction-card').style.display = 'block';
            setHtml('action-description', instructions);
          };

          /* 
           * TODO: This attaches validation to numeric form-inputs. This can be generalized 
           */
          for (const input of document.getElementsByClassName('gestalt-number')) {
            input.oninvalid = invalidNumber;
            input.oninput = resetValidation;
          }

          const element = tag => document.createElement(tag);

          /**
           * Set text on the input form.
           */
          const withFormTitle = (title) => 
            setText('form-title', title);
          
          /**
           * Display the colored header at the top of the window with the given title
           */
          const withHeader = (title) => {
            let text = title || 'New Action';
            get('gf-action-header').style.display = 'block';
            setText('gf-header-title', text);
          };

          /**
           * Build and display Resource-Info card from object-literal
           */
          const withResourceInfo = (info) => {
            let card = element('div');
            card.classList.add('col', 'card-panel','s12', 'resource-info');

            let table = element('table');
            let thead = element('thead');
            let tbody = element('tbody');
            let headrow = element('tr');
            let bodyrow = element('tr');

            Object.entries(info).forEach(([key,value]) => {
              let th = element('th');
              let td = element('td');
              
              th.appendChild(document.createTextNode(key));
              td.appendChild(document.createTextNode(value));

              headrow.appendChild(th);
              bodyrow.appendChild(td);
            });

            thead.appendChild(headrow);
            tbody.appendChild(bodyrow);
            table.appendChild(thead);
            table.appendChild(tbody);
            card.appendChild(table);
            table.classList.add('responsive-table');
            let div = get('dynamic-table');

            div.appendChild(card);
            get('resource-data').style.display = 'block';
          }
          
          //const token = getToken();
          
          /**
           * Close the gestalt-ui modal window.
           */
          function closeParent() {
            window.parent.document.getElementById('close-parent-modal').click();
          }

          /**
           * Determine if the given form-field should be serialized to the response payload.
           * The implementation will currently exclude any form element without an .id or .value -
           * this means that empty form elements will be excluded. If you want empty elements you'll
           * have to change the guard.
           */
          const includeFormField = field => {
            return field.id && field.value;
          }

          /**
           * Convert 'gestalt-action-form' fields to JSON using `input.id` as the keys.
           */  
          const actionJson = fields => [].reduce.call(fields, (data, field) => {
            if (includeFormField(field)) {
              data[field.id] = field.value;
            }
            return data;
          }, {});
          
          /**
           * Generate payload from form-data, POST to Meta, and close modal.
           */
          const invokeAction = event => {
            event.preventDefault();

            let form = get(GF_CONTROLS_FORM);
            let data = actionJson(form.elements);
            let payload = JSON.stringify(_ctx, null, " ");
            
            console.log("POST-PAYLOAD:\n" + payload);
            console.log("Calling __metaPost...");
            __metaPost('', _url, payload);
          };
          
          /**
           * Call Meta invoke funcdtion. POSTs form-data to /invoke endpoint.
           */
          const __metaPost = (token, url, payload) =>
            fetch(url, {
              method: 'POST',
              headers: {
                'Authorization': `${token}`,
                'Content-Type': 'application/json'
              },
              body: JSON.stringify(payload)
            }).then(response => response.json());

          /* Hook form 'submit' to custom function */
          //const form = document.getElementById(GF_CONTROLS_FORM);             
          //form.addEventListener("submit", invokeAction);     
            
        const testInvoke = function(url, token, event) {

          const opts = {
            method: 'POST',
            headers: {
              'Authorization': token
            }
          }
          
        	const res = 
          	fetch(_ctx.invoke_url, opts) 
              .then(response => response.json())
              .then(data => window.parent.postMessage(data, "*"))
              .catch(error => console.error(error))
        	
        	event.preventDefault();        	
        }

        const __bindSubmit = function(url, token) {
          const form = document.getElementById(GF_CONTROLS_FORM); 
          form.addEventListener("submit", testInvoke.bind(this, url, token), true);    
        }
            
        const triggerModal = (id) => $(id).modal().modal('open');

        /**
         *  
         */
         const setErrorControls = () => {
          let cn = get(GF_CONTROLS_CANCEL);
          cn.classList.remove("gf-blue");
          cn.classList.add('red', 'animated', 'twister');

          get(GF_CONTROLS_EXECUTE).disabled = true;
        }

        /** 
         * Display the error modal at the bottom of the page.
         */
        const throwError = (data, isFatal) => {
          setText('error-title', data.title);
          setHtml('error-body', data.body);

          let fatal = isFatal || false;
          if (fatal) {
            setErrorControls();
          }
          triggerModal('#gestalt-error');
        }

        /**
         * Open the splash modal with the given title and body.
         */
        const splash = (data) => {
          setText('gf-splash-title', data.title);
          setHtml('gf-splash-body', data.body);
          triggerModal('#gf-splash-screen');
        };           