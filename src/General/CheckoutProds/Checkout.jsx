import React,{ useState, useEffect } from 'react'
import { Paper, Stepper, Step, StepLabel, Typography, CircularProgress, Divider, Button, CssBaseline } from '@material-ui/core';
import { Link, useHistory } from 'react-router-dom';

import useStyles from './checkoutstyle'
import AddressForm from './CheckoutForm/AddressForm';
import PaymentForm from './CheckoutForm/PaymentForm';
import { commerce } from '../../lib/commerce';
 
const steps = ['Shipping address', 'Payment details']

const Checkout = ({ cart, order, onCaptureCheckout, error }) => {
    const [activeStep,setActiveStep] = useState(0);
    const [checkoutToken, setCheckoutToken] = useState(null);
    const [shippingData, setShippingData] = useState({});
    const [isFinished, setIsFinished] = useState(false);
    const classes = useStyles();
    const history = useHistory();

    useEffect(() => {

        if (cart.id) {
            const generateToken = async () => {
              try {
                const SetupToken = await commerce.checkout.generateToken(cart.id, { type: 'cart' });
                   
                
                
                   setCheckoutToken(SetupToken);
              
              } catch {
                if (activeStep !== steps.length) history.push('/');
              }
            };
      
            generateToken();
        }
    }, [cart]);

    const nextStep = () => setActiveStep((preActiveStep) => preActiveStep + 1);
    const backStep = () => setActiveStep((preActiveStep) => preActiveStep - 1);

    const next = (data) => {
        setShippingData(data);

        nextStep();
    }

    const timeout = () => {
        setTimeout(() => {
            setIsFinished(true);
        }, 3000)
    }
    let Confirmation = () => order.customer ? (
        <>
             <div>
                <Typography variant='h5'>Thank you for your purchase, {order.customer.firstname} {order.customer.lastname}</Typography>
                <Divider className={classes.divider}></Divider>
                <Typography variant='subtitle2'>Order ref: {order.customer_reference}</Typography>
             </div>
             <br/>
             <Button component={Link} to="/" variant="outlined" type="button">Back to Home</Button>
             </>
    ) : isFinished ? (
        <>
        <div>
      <Typography variant='h5'>Thank you for your purchase</Typography>
                <Divider className={classes.divider}></Divider>
                <Typography variant='subtitle2'>Order ref: {order.customer_reference}</Typography>
        </div>
        <Button component={Link} to="/" variant="outlined" type="button">Back to Home</Button>
        </>
        ) : (
        <div className={classes.spinner}>
            <CircularProgress></CircularProgress>
           
        </div>

    );

    if (error){
        <>
        <Typography variant='h5'>Error: {error}</Typography>
        </>
    }

    const Form = () => activeStep === 0
    ? <AddressForm checkoutToken={checkoutToken} next={next}/>
    : <PaymentForm checkoutToken={checkoutToken} shippingData={shippingData} backStep={backStep} onCaptureCheckout={onCaptureCheckout} nextStep={nextStep} timeout={timeout} />

    

    return (
        <>
        <CssBaseline></CssBaseline>
        <div className={classes.toolbar}></div>
        <main className={classes.layout}>
          <Paper className={classes.paper}>
                <Typography variant="h4" align="center">Checkout</Typography>
                <Stepper activeStep={activeStep} className={classes.stepper}>
                       {steps.map((step) => (
                        
                            <Step key={step}>
                                <StepLabel>{step}</StepLabel>
                            </Step>
                           
                       ))}

                </Stepper>
               { activeStep === steps.length ? <Confirmation/> : checkoutToken && <Form/>}
                 </Paper>
                 </main>
        
        </>
    )
}
export default Checkout;