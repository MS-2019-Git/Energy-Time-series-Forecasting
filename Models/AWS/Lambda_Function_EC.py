
import os
import boto3
import json

# grab environment variables
ENDPOINT_NAME = os.environ['ENDPOINT_NAME']
runtime= boto3.client('runtime.sagemaker')

def lambda_handler(event, context):
    print("Received event: " , type(event))
    
    data = json.loads(event['body'])
    #json.loads(json.dumps(event))
    payload = data["item_id"]+ ","+ data["timestamp"]
    #print(payload)
    
    response = runtime.invoke_endpoint(EndpointName=ENDPOINT_NAME,
                                       ContentType='text/csv',
                                       Body=payload)
    
    
   
    
    return "{ \"Forecast\":" + response['Body'].read().decode("utf-8") + "}"