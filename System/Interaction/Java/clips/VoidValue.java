package clips;

public class VoidValue extends PrimitiveValue
{
	/***************/
	/* VoidValue: */
	/***************/
	public VoidValue()
	{
		super(null);
	}

	/************/
	/* retain: */
	/************/
	public void retain()
	{
		//System.out.println("VoidValue retain");
	}

	/************/
	/* release: */
	/************/
	public void release()
	{
		//System.out.println("VoidValue release");
	}

}
