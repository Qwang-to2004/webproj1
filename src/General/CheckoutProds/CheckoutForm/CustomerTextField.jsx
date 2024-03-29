import React from 'react';
import { TextField, Grid } from '@material-ui/core';
import { useFormContext, Controller } from 'react-hook-form';

  const FormInput = ({ name, label }) => {
  const { control } = useFormContext();
  

  return (
    <Grid item sx={12} sm={6}>
     <Controller
         render={({ field }) => ( 
            <TextField {...field} label={label} required/>)}
            control={control}
            fullWidth
            name={name}>
                
     </Controller>
      
    </Grid>
  );
}

export default FormInput;
