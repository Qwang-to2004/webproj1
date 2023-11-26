import { makeStyles } from '@material-ui/core/styles';


export default makeStyles((theme) => ({
  toolbar: theme.mixins.toolbar,
  content: {
    flexGrow: 1,
    backgroundColor: theme.palette.background.default,
    backgroundImage: `url(${"https://i.pinimg.com/originals/0d/fe/0a/0dfe0ab9e655c284ab6008d9088f803a.jpg"})`,
    backgroundSize: 1500,
    padding: theme.spacing(3),
  },
  root: {
    flexGrow: 1,
  },
}));
